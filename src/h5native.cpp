// Native HDF5 backend for readNSx, built on the `hdf5lib` static HDF5 library.
//
// The on-disk layout produced here is the one `hdf5r` established, so files
// written by readNSx stay readable by `hdf5r`, `h5py`, and the HDF5
// command-line tools, and files written by older versions of readNSx are read
// back unchanged:
//
//   * the HDF5 dataspace dimensions are the *reverse* of the R dimensions, and
//     the R column-major buffer is written verbatim (no transpose)
//   * double    -> H5T_IEEE_F64LE
//   * integer   -> H5T_STD_I32LE
//   * character -> H5T_C_S1, variable length, null-terminated, ASCII
//   * logical   -> H5T_ENUM over H5T_STD_U8LE with FALSE=0, TRUE=1, NA=2
//   * datasets written whole get unlimited max dimensions; pre-allocated
//     datasets get fixed max dimensions
//
// Dimensions, offsets and counts cross the R boundary as doubles in R order, so
// datasets longer than 2^31 elements are addressable.

#include <cpp11.hpp>
#include <hdf5.h>

#include <algorithm>
#include <string>
#include <vector>

namespace {

// -----------------------------------------------------------------------------
// Handle management
// -----------------------------------------------------------------------------

// cpp11 converts R errors into C++ exceptions (cpp11::unwind_exception) before
// unwinding, so destructors do run and these guards are safe across stop().
template <herr_t (*Closer)(hid_t)>
class H5Handle {
 public:
  H5Handle() : id_(H5I_INVALID_HID) {}
  explicit H5Handle(hid_t id) : id_(id) {}
  ~H5Handle() { reset(); }

  H5Handle(const H5Handle&) = delete;
  H5Handle& operator=(const H5Handle&) = delete;

  // Movable so that helpers can hand a handle back by value.
  H5Handle(H5Handle&& other) noexcept : id_(other.id_) { other.id_ = H5I_INVALID_HID; }
  H5Handle& operator=(H5Handle&& other) noexcept {
    if (this != &other) {
      reset();
      id_ = other.id_;
      other.id_ = H5I_INVALID_HID;
    }
    return *this;
  }

  H5Handle& operator=(hid_t id) {
    reset();
    id_ = id;
    return *this;
  }

  void reset() {
    if (id_ >= 0) {
      Closer(id_);
      id_ = H5I_INVALID_HID;
    }
  }

  hid_t get() const { return id_; }
  operator hid_t() const { return id_; }
  bool valid() const { return id_ >= 0; }

 private:
  hid_t id_;
};

using FileHandle = H5Handle<H5Fclose>;
using DatasetHandle = H5Handle<H5Dclose>;
using SpaceHandle = H5Handle<H5Sclose>;
using TypeHandle = H5Handle<H5Tclose>;
using PropHandle = H5Handle<H5Pclose>;

// Silences the multi-page diagnostic stack HDF5 prints to stderr by default;
// failures are reported as R conditions instead.
struct ErrorSilencer {
  ErrorSilencer() { H5Eset_auto(H5E_DEFAULT, NULL, NULL); }
};

void quiet_hdf5() { static ErrorSilencer silencer; }

[[noreturn]] void fail(const std::string& what) { cpp11::stop("%s", what.c_str()); }

// -----------------------------------------------------------------------------
// Dimension helpers. R dimensions and HDF5 file dimensions are reverses of each
// other; everything crossing the R boundary is in R order.
// -----------------------------------------------------------------------------

std::vector<hsize_t> to_hsize(const cpp11::doubles& x) {
  std::vector<hsize_t> out;
  out.reserve(static_cast<size_t>(x.size()));
  for (R_xlen_t i = 0; i < x.size(); i++) {
    double v = x[i];
    if (!R_FINITE(v) || v < 0) {
      fail("HDF5 dimensions must be finite non-negative numbers");
    }
    out.push_back(static_cast<hsize_t>(v));
  }
  return out;
}

std::vector<hsize_t> reversed(const std::vector<hsize_t>& x) {
  return std::vector<hsize_t>(x.rbegin(), x.rend());
}

hsize_t product(const std::vector<hsize_t>& x) {
  hsize_t n = 1;
  for (size_t i = 0; i < x.size(); i++) n *= x[i];
  return n;
}

// R-order dimensions of an open dataset.
std::vector<hsize_t> dataset_dims(hid_t dset) {
  SpaceHandle space(H5Dget_space(dset));
  if (!space.valid()) fail("Cannot read the HDF5 dataspace");
  int rank = H5Sget_simple_extent_ndims(space);
  if (rank < 0) fail("Cannot determine the HDF5 dataset rank");
  std::vector<hsize_t> file_dims(static_cast<size_t>(rank));
  if (rank > 0 && H5Sget_simple_extent_dims(space, file_dims.data(), NULL) < 0) {
    fail("Cannot determine the HDF5 dataset dimensions");
  }
  return reversed(file_dims);
}

// -----------------------------------------------------------------------------
// Datatypes
// -----------------------------------------------------------------------------

// H5T_ENUM {FALSE: 0, TRUE: 1, NA: 2} over an 8-bit base, matching hdf5r.
hid_t make_logical_type(hid_t base) {
  hid_t type = H5Tenum_create(base);
  if (type < 0) return type;
  unsigned char v;
  v = 0; H5Tenum_insert(type, "FALSE", &v);
  v = 1; H5Tenum_insert(type, "TRUE", &v);
  v = 2; H5Tenum_insert(type, "NA", &v);
  return type;
}

// Variable-length, null-terminated UTF-8. UTF-8 is a superset of ASCII, so for
// plain ASCII text the stored bytes are the same ones hdf5r writes; only the
// character-set label in the datatype differs. Reading always follows whatever
// the file itself declares, so legacy CSET_ASCII datasets are unaffected.
hid_t make_string_type() {
  hid_t type = H5Tcopy(H5T_C_S1);
  if (type < 0) return type;
  H5Tset_size(type, H5T_VARIABLE);
  H5Tset_strpad(type, H5T_STR_NULLTERM);
  H5Tset_cset(type, H5T_CSET_UTF8);
  return type;
}

// R complex is `struct { double r; double i; }`, so this compound describes the
// R buffer exactly and needs no conversion in either direction. The member
// names are numpy's, which makes the dataset a native `complex128` to h5py.
hid_t make_complex_type(hid_t base, const char* real_name, const char* imag_name) {
  hid_t type = H5Tcreate(H5T_COMPOUND, 2 * sizeof(double));
  if (type < 0) return type;
  H5Tinsert(type, real_name, 0, base);
  H5Tinsert(type, imag_name, sizeof(double), base);
  return type;
}

// The datatype stored in the file for a given R type.
hid_t file_type_for(SEXPTYPE sexp_type) {
  switch (sexp_type) {
    case REALSXP: return H5Tcopy(H5T_IEEE_F64LE);
    case INTSXP: return H5Tcopy(H5T_STD_I32LE);
    case LGLSXP: return make_logical_type(H5T_STD_U8LE);
    case STRSXP: return make_string_type();
    case CPLXSXP: return make_complex_type(H5T_IEEE_F64LE, "r", "i");
    default: return H5I_INVALID_HID;
  }
}

// The datatype describing the in-memory R buffer.
hid_t memory_type_for(SEXPTYPE sexp_type) {
  switch (sexp_type) {
    case REALSXP: return H5Tcopy(H5T_NATIVE_DOUBLE);
    case INTSXP: return H5Tcopy(H5T_NATIVE_INT);
    case LGLSXP: return make_logical_type(H5T_NATIVE_UCHAR);
    case STRSXP: return make_string_type();
    case CPLXSXP: return make_complex_type(H5T_NATIVE_DOUBLE, "r", "i");
    default: return H5I_INVALID_HID;
  }
}

SEXPTYPE sexp_type_from_string(const std::string& ctype) {
  if (ctype == "numeric" || ctype == "double" || ctype == "float") return REALSXP;
  if (ctype == "integer") return INTSXP;
  if (ctype == "logical") return LGLSXP;
  if (ctype == "character" || ctype == "string") return STRSXP;
  if (ctype == "complex") return CPLXSXP;
  fail("Unsupported HDF5 storage type `" + ctype + "`");
}

// Types whose R buffer can be handed to HDF5 as-is, given a matching memory
// type: integer, double and complex.
const void* raw_buffer_of(SEXP x, SEXPTYPE sexp_type) {
  switch (sexp_type) {
    case INTSXP: return INTEGER(x);
    case CPLXSXP: return COMPLEX(x);
    default: return REAL(x);
  }
}

// A two-member compound of floats is how every common tool stores complex:
// h5py/numpy name the members "r"/"i", hdf5r names them "Real"/"Imaginary".
bool is_complex_compound(hid_t type) {
  if (H5Tget_class(type) != H5T_COMPOUND) { return false; }
  if (H5Tget_nmembers(type) != 2) { return false; }
  for (unsigned i = 0; i < 2; i++) {
    TypeHandle member(H5Tget_member_type(type, i));
    if (!member.valid() || H5Tget_class(member) != H5T_FLOAT) { return false; }
  }
  return true;
}

// Which R type a stored dataset should be read back as.
SEXPTYPE sexp_type_of(hid_t dset) {
  TypeHandle type(H5Dget_type(dset));
  if (!type.valid()) fail("Cannot read the HDF5 datatype");
  H5T_class_t cls = H5Tget_class(type);
  switch (cls) {
    case H5T_FLOAT: return REALSXP;
    case H5T_STRING: return STRSXP;
    case H5T_ENUM: {
      // Only the 3-member FALSE/TRUE/NA enum round-trips as logical. Member
      // order is not fixed - hdf5r writes FALSE/TRUE/NA, h5py sorts by name -
      // so match on the set of names and let HDF5 convert by name.
      if (H5Tget_nmembers(type) == 3) {
        int found = 0;
        for (unsigned i = 0; i < 3; i++) {
          char* name = H5Tget_member_name(type, i);
          if (name != NULL) {
            std::string nm(name);
            if (nm == "FALSE" || nm == "TRUE" || nm == "NA") found++;
            H5free_memory(name);
          }
        }
        if (found == 3) return LGLSXP;
      }
      return INTSXP;
    }
    case H5T_INTEGER: {
      // Anything wider than a 32-bit signed integer cannot be held by an R
      // integer, so fall back to double.
      size_t size = H5Tget_size(type);
      H5T_sign_t sign = H5Tget_sign(type);
      if (size < 4 || (size == 4 && sign == H5T_SGN_2)) return INTSXP;
      return REALSXP;
    }
    case H5T_BITFIELD: return INTSXP;
    case H5T_COMPOUND: {
      if (is_complex_compound(type)) { return CPLXSXP; }
      fail("Unsupported HDF5 compound datatype");
    }
#if H5_VERS_MAJOR >= 2
    // HDF5 2.0 gained a native complex class; read those too.
    case H5T_COMPLEX: return CPLXSXP;
#endif
    default: return REALSXP;
  }
}

// -----------------------------------------------------------------------------
// Chunking
// -----------------------------------------------------------------------------

const hsize_t kAutoChunkBytes = 1048576;  // ~1 MiB per chunk

// `chunk` arrives in R order and may be NA (meaning "auto") or a single value to
// be recycled across dimensions. Returns file-order chunk dimensions.
std::vector<hsize_t> resolve_chunk(const cpp11::doubles& chunk,
                                   const std::vector<hsize_t>& file_dims,
                                   size_t element_size, bool clamp_to_extent) {
  size_t rank = file_dims.size();
  std::vector<hsize_t> out(rank, 1);

  bool automatic = chunk.size() == 0;
  for (R_xlen_t i = 0; i < chunk.size() && !automatic; i++) {
    if (!R_FINITE(chunk[i]) || chunk[i] < 1) automatic = true;
  }

  if (!automatic) {
    // R order -> file order, recycling a scalar across every dimension.
    for (size_t i = 0; i < rank; i++) {
      R_xlen_t idx = chunk.size() == 1 ? 0 : static_cast<R_xlen_t>(rank - 1 - i);
      if (idx >= chunk.size()) idx = chunk.size() - 1;
      out[i] = static_cast<hsize_t>(chunk[idx]);
    }
  } else {
    for (size_t i = 0; i < rank; i++) out[i] = std::max<hsize_t>(file_dims[i], 1);
    if (element_size == 0) element_size = 1;
    hsize_t budget = std::max<hsize_t>(kAutoChunkBytes / element_size, 1);
    while (product(out) > budget) {
      // Halve the largest dimension until the chunk fits the budget.
      size_t largest = 0;
      for (size_t i = 1; i < rank; i++) {
        if (out[i] > out[largest]) largest = i;
      }
      if (out[largest] <= 1) break;
      out[largest] = (out[largest] + 1) / 2;
    }
  }

  for (size_t i = 0; i < rank; i++) {
    if (out[i] < 1) out[i] = 1;
    // HDF5 rejects chunks larger than the extent unless the dimension can grow.
    if (clamp_to_extent && file_dims[i] > 0 && out[i] > file_dims[i]) {
      out[i] = file_dims[i];
    }
  }
  return out;
}

PropHandle make_create_plist(const std::vector<hsize_t>& chunk, int level) {
  PropHandle dcpl(H5Pcreate(H5P_DATASET_CREATE));
  if (!dcpl.valid()) fail("Cannot create an HDF5 property list");
  if (H5Pset_chunk(dcpl, static_cast<int>(chunk.size()), chunk.data()) < 0) {
    fail("Cannot set the HDF5 chunk size");
  }
  if (level < 0) level = 0;
  if (level > 9) level = 9;
  // hdf5r installs the deflate filter even at level 0; mirror that.
  H5Pset_deflate(dcpl, static_cast<unsigned>(level));
  return dcpl;
}

// -----------------------------------------------------------------------------
// File / link helpers
// -----------------------------------------------------------------------------

FileHandle open_file(const std::string& path, bool writable) {
  quiet_hdf5();
  unsigned flags = writable ? H5F_ACC_RDWR : H5F_ACC_RDONLY;
  FileHandle file(H5Fopen(path.c_str(), flags, H5P_DEFAULT));
  return file;
}

// Opens for writing, creating the file when it does not exist yet.
FileHandle open_or_create(const std::string& path) {
  quiet_hdf5();
  htri_t is_h5 = H5Fis_accessible(path.c_str(), H5P_DEFAULT);
  FileHandle file;
  if (is_h5 > 0) {
    file = H5Fopen(path.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
  } else {
    file = H5Fcreate(path.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  }
  if (!file.valid()) {
    fail("Cannot open HDF5 file for writing: " + path);
  }
  return file;
}

bool link_exists(hid_t file, const std::string& name) {
  // Every ancestor must exist before H5Lexists may be asked about a nested path.
  std::string path;
  size_t pos = 0;
  while (pos < name.size()) {
    size_t next = name.find('/', pos);
    std::string part = name.substr(pos, next == std::string::npos ? std::string::npos : next - pos);
    pos = (next == std::string::npos) ? name.size() : next + 1;
    if (part.empty()) continue;
    path += "/" + part;
    htri_t exists = H5Lexists(file, path.c_str(), H5P_DEFAULT);
    if (exists <= 0) return false;
  }
  return !path.empty();
}

PropHandle make_link_plist() {
  PropHandle lcpl(H5Pcreate(H5P_LINK_CREATE));
  if (!lcpl.valid()) fail("Cannot create an HDF5 link property list");
  H5Pset_create_intermediate_group(lcpl, 1);
  return lcpl;
}

DatasetHandle open_dataset(hid_t file, const std::string& name, const std::string& path) {
  if (!link_exists(file, name)) {
    fail("File [" + path + "] has no [" + name + "] in it.");
  }
  DatasetHandle dset(H5Dopen(file, name.c_str(), H5P_DEFAULT));
  if (!dset.valid()) {
    fail("File [" + path + "] has no [" + name + "] in it.");
  }
  return dset;
}

// -----------------------------------------------------------------------------
// Reading
// -----------------------------------------------------------------------------

// Reads `n` elements of `dset` selected by `fspace` into a fresh R vector.
SEXP read_into_r(hid_t dset, hid_t fspace, hid_t mspace, hsize_t n,
                 const std::vector<hsize_t>& r_dims, bool set_dim) {
  SEXPTYPE sexp_type = sexp_type_of(dset);
  R_xlen_t len = static_cast<R_xlen_t>(n);

  cpp11::sexp result;

  if (sexp_type == STRSXP) {
    TypeHandle file_type(H5Dget_type(dset));
    htri_t is_vlen = H5Tis_variable_str(file_type);
    // Mark the R strings UTF-8 only when the file says so, which is what hdf5r
    // does; legacy CSET_ASCII datasets keep coming back unmarked.
    cetype_t enc = (H5Tget_cset(file_type) == H5T_CSET_UTF8) ? CE_UTF8 : CE_NATIVE;
    cpp11::writable::strings out(len);
    if (is_vlen > 0) {
      // Reuse the file's own string type as the memory type. Hardcoding a
      // character set here would make HDF5 refuse files written with another
      // one - h5py, for instance, writes UTF-8 where hdf5r writes ASCII.
      TypeHandle mem_type(H5Tcopy(file_type));
      std::vector<char*> buffer(static_cast<size_t>(len), NULL);
      if (len > 0 && H5Dread(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer.data()) < 0) {
        fail("Cannot read the HDF5 string dataset");
      }
      for (R_xlen_t i = 0; i < len; i++) {
        // A NULL element is how a missing string is stored on disk.
        out[i] = buffer[static_cast<size_t>(i)] == NULL
                     ? NA_STRING
                     : Rf_mkCharCE(buffer[static_cast<size_t>(i)], enc);
      }
      if (len > 0) {
        // H5Treclaim needs a real dataspace describing the buffer; H5S_ALL is
        // not one, so make a matching flat space when reading everything.
        SpaceHandle flat;
        hid_t reclaim_space = mspace;
        if (mspace == H5S_ALL) {
          hsize_t count = static_cast<hsize_t>(len);
          flat = H5Screate_simple(1, &count, NULL);
          reclaim_space = flat;
        }
        H5Treclaim(mem_type, reclaim_space, H5P_DEFAULT, buffer.data());
      }
    } else {
      size_t width = H5Tget_size(file_type);
      if (width == 0) fail("Cannot determine the HDF5 string width");
      TypeHandle mem_type(H5Tcopy(file_type));
      H5Tset_size(mem_type, width + 1);
      H5Tset_strpad(mem_type, H5T_STR_NULLTERM);
      std::vector<char> buffer(static_cast<size_t>(len) * (width + 1), '\0');
      if (len > 0 && H5Dread(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer.data()) < 0) {
        fail("Cannot read the HDF5 string dataset");
      }
      for (R_xlen_t i = 0; i < len; i++) {
        out[i] = Rf_mkCharCE(&buffer[static_cast<size_t>(i) * (width + 1)], enc);
      }
    }
    result = out;
  } else if (sexp_type == CPLXSXP) {
    TypeHandle file_type(H5Dget_type(dset));
    TypeHandle mem_type;
    if (is_complex_compound(file_type)) {
      // HDF5 converts compounds member by member, matched on name, so the
      // memory type has to reuse the names the file happens to use - "r"/"i"
      // from h5py, "Real"/"Imaginary" from hdf5r.
      char* real_name = H5Tget_member_name(file_type, 0);
      char* imag_name = H5Tget_member_name(file_type, 1);
      if (real_name == NULL || imag_name == NULL) {
        if (real_name != NULL) H5free_memory(real_name);
        if (imag_name != NULL) H5free_memory(imag_name);
        fail("Cannot read the HDF5 complex member names");
      }
      mem_type = make_complex_type(H5T_NATIVE_DOUBLE, real_name, imag_name);
      H5free_memory(real_name);
      H5free_memory(imag_name);
    } else {
      mem_type = make_complex_type(H5T_NATIVE_DOUBLE, "r", "i");
    }
    if (!mem_type.valid()) fail("Cannot construct the HDF5 datatype");

    // Rcomplex is {double r; double i;}, so this reads straight into the vector.
    // cpp11 has no complex vector type, so allocate one and let cpp11::sexp
    // hold the protection.
    cpp11::sexp out(Rf_allocVector(CPLXSXP, len));
    if (len > 0 && H5Dread(dset, mem_type, mspace, fspace, H5P_DEFAULT, COMPLEX(out)) < 0) {
      fail("Cannot read the HDF5 complex dataset");
    }
    result = out;
  } else if (sexp_type == LGLSXP) {
    TypeHandle mem_type(make_logical_type(H5T_NATIVE_UCHAR));
    std::vector<unsigned char> buffer(static_cast<size_t>(len), 0);
    if (len > 0 && H5Dread(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer.data()) < 0) {
      fail("Cannot read the HDF5 logical dataset");
    }
    cpp11::writable::logicals out(len);
    for (R_xlen_t i = 0; i < len; i++) {
      unsigned char v = buffer[static_cast<size_t>(i)];
      out[i] = (v == 2) ? NA_LOGICAL : (v != 0);
    }
    result = out;
  } else if (sexp_type == INTSXP) {
    cpp11::writable::integers out(len);
    if (len > 0 && H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, INTEGER(out)) < 0) {
      fail("Cannot read the HDF5 integer dataset");
    }
    result = out;
  } else {
    cpp11::writable::doubles out(len);
    if (len > 0 && H5Dread(dset, H5T_NATIVE_DOUBLE, mspace, fspace, H5P_DEFAULT, REAL(out)) < 0) {
      fail("Cannot read the HDF5 dataset");
    }
    result = out;
  }

  // hdf5r returns a plain vector for rank-1 datasets and an array otherwise.
  if (set_dim && r_dims.size() > 1) {
    cpp11::writable::integers dim(static_cast<R_xlen_t>(r_dims.size()));
    for (size_t i = 0; i < r_dims.size(); i++) {
      dim[static_cast<R_xlen_t>(i)] = static_cast<int>(r_dims[i]);
    }
    Rf_setAttrib(result, R_DimSymbol, dim);
  }
  return result;
}

}  // namespace

// -----------------------------------------------------------------------------
// Registered entry points
// -----------------------------------------------------------------------------

[[cpp11::register]]
bool h5_native_is_h5(std::string path) {
  quiet_hdf5();
  htri_t re = H5Fis_accessible(path.c_str(), H5P_DEFAULT);
  return re > 0;
}

[[cpp11::register]]
cpp11::writable::strings h5_native_list_datasets(std::string path) {
  FileHandle file = open_file(path, false);
  if (!file.valid()) fail("Cannot open HDF5 file: " + path);

  std::vector<std::string> names;
  auto collect = [](hid_t, const char* name, const H5O_info2_t* info,
                    void* op_data) -> herr_t {
    if (info != NULL && info->type == H5O_TYPE_DATASET) {
      std::string nm(name);
      if (nm != ".") {
        static_cast<std::vector<std::string>*>(op_data)->push_back(nm);
      }
    }
    return 0;
  };
  if (H5Ovisit(file, H5_INDEX_NAME, H5_ITER_INC, collect, &names, H5O_INFO_BASIC) < 0) {
    fail("Cannot list the datasets in " + path);
  }

  cpp11::writable::strings out(static_cast<R_xlen_t>(names.size()));
  for (size_t i = 0; i < names.size(); i++) {
    out[static_cast<R_xlen_t>(i)] = names[i];
  }
  return out;
}

[[cpp11::register]]
bool h5_native_exists(std::string path, std::string name) {
  quiet_hdf5();
  if (H5Fis_accessible(path.c_str(), H5P_DEFAULT) <= 0) return false;
  FileHandle file = open_file(path, false);
  if (!file.valid()) return false;
  return link_exists(file, name);
}

[[cpp11::register]]
cpp11::writable::doubles h5_native_dims(std::string path, std::string name) {
  FileHandle file = open_file(path, false);
  if (!file.valid()) fail("Cannot open HDF5 file: " + path);
  DatasetHandle dset = open_dataset(file, name, path);
  std::vector<hsize_t> dims = dataset_dims(dset);

  cpp11::writable::doubles out(static_cast<R_xlen_t>(dims.size()));
  for (size_t i = 0; i < dims.size(); i++) {
    out[static_cast<R_xlen_t>(i)] = static_cast<double>(dims[i]);
  }
  return out;
}

[[cpp11::register]]
SEXP h5_native_read(std::string path, std::string name) {
  FileHandle file = open_file(path, false);
  if (!file.valid()) fail("Cannot open HDF5 file: " + path);
  DatasetHandle dset = open_dataset(file, name, path);
  std::vector<hsize_t> dims = dataset_dims(dset);
  return read_into_r(dset, H5S_ALL, H5S_ALL, product(dims), dims, true);
}

[[cpp11::register]]
SEXP h5_native_read_slab(std::string path, std::string name, cpp11::doubles start,
                         cpp11::doubles count) {
  FileHandle file = open_file(path, false);
  if (!file.valid()) fail("Cannot open HDF5 file: " + path);
  DatasetHandle dset = open_dataset(file, name, path);

  std::vector<hsize_t> r_dims = dataset_dims(dset);
  std::vector<hsize_t> r_start = to_hsize(start);
  std::vector<hsize_t> r_count = to_hsize(count);

  if (r_start.size() != r_dims.size() || r_count.size() != r_dims.size()) {
    fail("C++ `h5_native_read_slab`: `start` and `count` must match the rank of the dataset");
  }
  for (size_t i = 0; i < r_dims.size(); i++) {
    if (r_start[i] < 1 || r_start[i] - 1 + r_count[i] > r_dims[i]) {
      fail("C++ `h5_native_read_slab`: HDF5 read is out of bounds");
    }
  }

  // R order -> file order, and 1-based -> 0-based.
  std::vector<hsize_t> f_start(r_dims.size());
  std::vector<hsize_t> f_count(r_dims.size());
  for (size_t i = 0; i < r_dims.size(); i++) {
    f_start[r_dims.size() - 1 - i] = r_start[i] - 1;
    f_count[r_dims.size() - 1 - i] = r_count[i];
  }

  SpaceHandle fspace(H5Dget_space(dset));
  if (!fspace.valid()) fail("Cannot read the HDF5 dataspace");
  if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, f_start.data(), NULL, f_count.data(),
                          NULL) < 0) {
    fail("Cannot select the HDF5 hyperslab");
  }
  SpaceHandle mspace(H5Screate_simple(static_cast<int>(f_count.size()), f_count.data(), NULL));
  if (!mspace.valid()) fail("Cannot create the HDF5 memory dataspace");

  return read_into_r(dset, fspace, mspace, product(r_count), r_count, true);
}

[[cpp11::register]]
std::string h5_native_write(std::string path, std::string name, SEXP x, cpp11::doubles chunk,
                            int level, bool replace) {
  SEXPTYPE sexp_type = TYPEOF(x);
  if (sexp_type != REALSXP && sexp_type != INTSXP && sexp_type != LGLSXP &&
      sexp_type != STRSXP && sexp_type != CPLXSXP) {
    fail("Cannot write this data type to HDF5");
  }

  // R order dimensions of `x`; a dimensionless vector is rank 1.
  std::vector<hsize_t> r_dims;
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (dim != R_NilValue) {
    for (R_xlen_t i = 0; i < Rf_xlength(dim); i++) {
      r_dims.push_back(static_cast<hsize_t>(INTEGER(dim)[i]));
    }
  } else {
    r_dims.push_back(static_cast<hsize_t>(Rf_xlength(x)));
  }
  std::vector<hsize_t> f_dims = reversed(r_dims);

  FileHandle file = open_or_create(path);
  if (link_exists(file, name)) {
    if (!replace) fail("C++ `h5_native_write`: dataset already exists; use `replace = TRUE`");
    if (H5Ldelete(file, name.c_str(), H5P_DEFAULT) < 0) {
      fail("Cannot remove the existing HDF5 dataset");
    }
  }

  TypeHandle file_type(file_type_for(sexp_type));
  TypeHandle mem_type(memory_type_for(sexp_type));
  if (!file_type.valid() || !mem_type.valid()) {
    fail("Cannot construct the HDF5 datatype");
  }

  // Written-whole datasets get unlimited max dimensions, matching hdf5r; this is
  // also what makes a chunk larger than the extent legal.
  std::vector<hsize_t> maxdims(f_dims.size(), H5S_UNLIMITED);
  SpaceHandle space(
      H5Screate_simple(static_cast<int>(f_dims.size()), f_dims.data(), maxdims.data()));
  if (!space.valid()) fail("Cannot create the HDF5 dataspace");

  size_t element_size = (sexp_type == STRSXP) ? sizeof(char*) : H5Tget_size(file_type);
  std::vector<hsize_t> chunk_dims = resolve_chunk(chunk, f_dims, element_size, false);
  PropHandle dcpl = make_create_plist(chunk_dims, level);
  PropHandle lcpl = make_link_plist();

  DatasetHandle dset(
      H5Dcreate(file, name.c_str(), file_type, space, lcpl, dcpl, H5P_DEFAULT));
  if (!dset.valid()) fail("Cannot create the HDF5 dataset");

  R_xlen_t len = Rf_xlength(x);
  if (len > 0) {
    herr_t status = -1;
    if (sexp_type == STRSXP) {
      std::vector<const char*> buffer(static_cast<size_t>(len));
      for (R_xlen_t i = 0; i < len; i++) {
        SEXP elt = STRING_ELT(x, i);
        // A NULL pointer is HDF5's own representation of a missing string, and
        // is what makes NA_character_ survive a round trip. (hdf5r instead
        // stores the literal text "NA", which cannot be told apart from a real
        // "NA" string; it reads a NULL back as "".)
        buffer[static_cast<size_t>(i)] =
            (elt == NA_STRING) ? NULL : Rf_translateCharUTF8(elt);
      }
      status = H5Dwrite(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer.data());
    } else if (sexp_type == LGLSXP) {
      std::vector<unsigned char> buffer(static_cast<size_t>(len));
      for (R_xlen_t i = 0; i < len; i++) {
        int v = LOGICAL(x)[i];
        buffer[static_cast<size_t>(i)] =
            (v == NA_LOGICAL) ? 2 : (v ? 1 : 0);
      }
      status = H5Dwrite(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer.data());
    } else {
      // Integer, double and complex go out verbatim: the memory type already
      // describes R's own layout, Rcomplex included.
      const void* buffer = raw_buffer_of(x, sexp_type);
      status = H5Dwrite(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    }
    if (status < 0) fail("Cannot write the HDF5 dataset");
  }

  return path;
}

[[cpp11::register]]
std::string h5_native_allocate(std::string path, std::string name, cpp11::doubles dims,
                               std::string ctype, cpp11::doubles chunk, int level,
                               bool replace) {
  SEXPTYPE sexp_type = sexp_type_from_string(ctype);
  std::vector<hsize_t> r_dims = to_hsize(dims);
  if (r_dims.empty()) fail("C++ `h5_native_allocate`: `dims` must have at least one dimension");
  std::vector<hsize_t> f_dims = reversed(r_dims);

  FileHandle file = open_or_create(path);
  if (link_exists(file, name)) {
    if (!replace) fail("Dataset already exists. Use replace = TRUE to overwrite.");
    if (H5Ldelete(file, name.c_str(), H5P_DEFAULT) < 0) {
      fail("Cannot remove the existing HDF5 dataset");
    }
  }

  TypeHandle file_type(file_type_for(sexp_type));
  if (!file_type.valid()) fail("Cannot construct the HDF5 datatype");

  // Pre-allocated datasets get fixed max dimensions, matching what the hdf5r
  // backend does via `H5S$new(dims = dims, maxdims = dims)`.
  SpaceHandle space(
      H5Screate_simple(static_cast<int>(f_dims.size()), f_dims.data(), f_dims.data()));
  if (!space.valid()) fail("Cannot create the HDF5 dataspace");

  size_t element_size = (sexp_type == STRSXP) ? sizeof(char*) : H5Tget_size(file_type);
  std::vector<hsize_t> chunk_dims = resolve_chunk(chunk, f_dims, element_size, true);
  PropHandle dcpl = make_create_plist(chunk_dims, level);
  PropHandle lcpl = make_link_plist();

  DatasetHandle dset(
      H5Dcreate(file, name.c_str(), file_type, space, lcpl, dcpl, H5P_DEFAULT));
  if (!dset.valid()) fail("Cannot create the HDF5 dataset");

  return path;
}

[[cpp11::register]]
std::string h5_native_write_slab(std::string path, std::string name, SEXP x,
                                 cpp11::doubles start) {
  SEXPTYPE sexp_type = TYPEOF(x);
  if (sexp_type != REALSXP && sexp_type != INTSXP && sexp_type != LGLSXP &&
      sexp_type != STRSXP && sexp_type != CPLXSXP) {
    fail("Cannot write this data type to HDF5");
  }

  FileHandle file = open_file(path, true);
  if (!file.valid()) {
    fail("Dataset does not exist. Call allocate_h5() first.");
  }
  DatasetHandle dset = open_dataset(file, name, path);

  std::vector<hsize_t> r_dims = dataset_dims(dset);
  std::vector<hsize_t> r_start = to_hsize(start);

  std::vector<hsize_t> r_count;
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (dim != R_NilValue) {
    for (R_xlen_t i = 0; i < Rf_xlength(dim); i++) {
      r_count.push_back(static_cast<hsize_t>(INTEGER(dim)[i]));
    }
  } else {
    r_count.push_back(static_cast<hsize_t>(Rf_xlength(x)));
  }

  if (r_start.size() != r_count.size()) {
    fail("start must have the same number of dimensions as the data");
  }
  if (r_start.size() != r_dims.size()) {
    fail("start must have the same number of dimensions as the data");
  }
  for (size_t i = 0; i < r_dims.size(); i++) {
    if (r_start[i] < 1 || r_start[i] - 1 + r_count[i] > r_dims[i]) {
      fail("HDF5 write is out of bounds");
    }
  }

  std::vector<hsize_t> f_start(r_dims.size());
  std::vector<hsize_t> f_count(r_dims.size());
  for (size_t i = 0; i < r_dims.size(); i++) {
    f_start[r_dims.size() - 1 - i] = r_start[i] - 1;
    f_count[r_dims.size() - 1 - i] = r_count[i];
  }

  SpaceHandle fspace(H5Dget_space(dset));
  if (!fspace.valid()) fail("Cannot read the HDF5 dataspace");
  if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, f_start.data(), NULL, f_count.data(),
                          NULL) < 0) {
    fail("Cannot select the HDF5 hyperslab");
  }
  SpaceHandle mspace(H5Screate_simple(static_cast<int>(f_count.size()), f_count.data(), NULL));
  if (!mspace.valid()) fail("Cannot create the HDF5 memory dataspace");

  TypeHandle mem_type(memory_type_for(sexp_type));
  if (!mem_type.valid()) fail("Cannot construct the HDF5 datatype");

  R_xlen_t len = Rf_xlength(x);
  herr_t status = 0;
  if (len > 0) {
    if (sexp_type == STRSXP) {
      std::vector<const char*> buffer(static_cast<size_t>(len));
      for (R_xlen_t i = 0; i < len; i++) {
        SEXP elt = STRING_ELT(x, i);
        // A NULL pointer is HDF5's own representation of a missing string, and
        // is what makes NA_character_ survive a round trip. (hdf5r instead
        // stores the literal text "NA", which cannot be told apart from a real
        // "NA" string; it reads a NULL back as "".)
        buffer[static_cast<size_t>(i)] =
            (elt == NA_STRING) ? NULL : Rf_translateCharUTF8(elt);
      }
      status = H5Dwrite(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer.data());
    } else if (sexp_type == LGLSXP) {
      std::vector<unsigned char> buffer(static_cast<size_t>(len));
      for (R_xlen_t i = 0; i < len; i++) {
        int v = LOGICAL(x)[i];
        buffer[static_cast<size_t>(i)] = (v == NA_LOGICAL) ? 2 : (v ? 1 : 0);
      }
      status = H5Dwrite(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer.data());
    } else {
      // Integer, double and complex go out verbatim: the memory type already
      // describes R's own layout, Rcomplex included.
      const void* buffer = raw_buffer_of(x, sexp_type);
      status = H5Dwrite(dset, mem_type, mspace, fspace, H5P_DEFAULT, buffer);
    }
  }
  if (status < 0) fail("Cannot write the HDF5 hyperslab");

  return path;
}

[[cpp11::register]]
bool h5_native_writable(std::string path) {
  quiet_hdf5();
  FileHandle file = open_or_create(path);
  return file.valid();
}
