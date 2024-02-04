#include <vector>
#include <map>
#include <string>
#include <cpp11.hpp>
#include "common.h"
using namespace cpp11;

template <class S>
S* getBCIObjPointer(SEXP s, bool checkZero=true)  // internal function
{
  if (TYPEOF(s) != EXTPTRSXP)
    ::Rf_error("not an external pointer");

  SEXP tag = R_ExternalPtrTag(s);  // our convention, this can be anything
  if (TYPEOF(tag) != CHARSXP || strcmp(CHAR(tag), "BCIObjClass") != 0)
    ::Rf_error("Not a BCI2000 object");

  S* sp = (S*)R_ExternalPtrAddr(s);
  if (checkZero && !sp)
    ::Rf_error("Address is 0");

  return sp;
}

template <class S>
void bciObjFinaliser(SEXP s)  // internal function
{
  // called during garbage collection
  S* sp = getBCIObjPointer<S>(s, false);
  if (sp) {
    // ::Rprintf("%s", "Destructing a BCI Object");
    delete sp;  // destruct S, release SEXPs
    R_ClearExternalPtr(s);
  }
}

template <class S>
SEXP bciObjCreate(const SEXP & config)
{
  S* sp = new S(config);  // Obj pointer
  SEXP s = PROTECT(
    R_MakeExternalPtr((void*)sp, Rf_mkChar("BCIObjClass"), R_NilValue)
  );
  R_RegisterCFinalizerEx(s, bciObjFinaliser<S>, TRUE);  // auto-called on GC

  SEXP clsNames = PROTECT(Rf_allocVector(STRSXP, 3));
  std::string cls_(sp->cls_);
  SET_STRING_ELT(clsNames, 0, Rf_mkChar( cls_.c_str() ));
  SET_STRING_ELT(clsNames, 1, Rf_mkChar("BCIObjClass"));
  SET_STRING_ELT(clsNames, 2, Rf_mkChar("externalptr"));
  Rf_setAttrib(s, R_ClassSymbol, clsNames);
  UNPROTECT(2);
  return s;
}


[[cpp11::register]]
std::string bciStrDecode(const std::string& x, const std::string& nil = "NA") {

  const char* x_ = x.c_str();
  const size_t xlen = x.length();
  char* xEnd = (char*)( x_ + xlen );

  char * ptr = (char*) x_;

  std::string re = "";
  char c = 0;

  while( ptr != xEnd && *ptr != '\0' ) {
    if( *ptr != '%' ) {
      re += *ptr++;
      continue;
    }
    ptr++;
    if( ptr == xEnd ) {
      re += nil;
      break;
    }
    if( *ptr == ' ' ) {
      re += nil + ' ';
      ptr++;
      continue;
    }
    if( *ptr == '%' ) {
      re += '%';
      ptr++;
      continue;
    }
    c = (char) strtol( ptr, &ptr, 16 );
    if( c == '\0' ) {
      re += nil;
    } else {
      re += c;
    }
  }
  return re;
}

struct BCIStateDef {
  std::string name;
  int len;
  int byteLoc;
  int bitLoc;
  std::uint64_t defaultVal;

  BCIStateDef(
    const std::string & name,
    const int & len,
    const int & byteLoc,
    const int & bitLoc,
    const std::uint64_t & defaultVal
  ) : name(name), len(len), byteLoc(byteLoc), bitLoc(bitLoc), defaultVal(defaultVal){
  };

  template <typename T>
  T parseInternal(const char* &ptr) {
    T value = 0;
    std::uint8_t* ptr2 = (std::uint8_t*) ptr + this->byteLoc;
    value += (*ptr2++) >> this->bitLoc;

    int len2 = 8 - this->bitLoc;

    for(; len2 < this->len; len2 += 8 ) {
      value += (*ptr2++) << len2;
    }
    // ::Rprintf("%llu ", (std::uint64_t) value);
    return value;
  }

  std::uint64_t parse(const char* &ptr) {
    if( this->len <= 8 ) {
      return (std::uint64_t) parseInternal<std::uint8_t>(ptr);
    }
    if( this->len <= 16 ) {
      return (std::uint64_t) parseInternal<std::uint16_t>(ptr);
    }
    if( this->len <= 32 ) {
      return (std::uint64_t) parseInternal<std::uint32_t>(ptr);
    }
    return parseInternal<std::uint64_t>(ptr);
  }

  std::string format(const std::string & indent) {
    return (indent + "[BCIStateDef: " + name + "\t] Len=" + std::to_string(len) +
            ", ByteLoc=" + std::to_string(byteLoc) +
            ", BitLoc=" + std::to_string(bitLoc) +
            ", DefaultValue=" + std::to_string(defaultVal) +
            "\r\n");
  }

  void print(const std::string & indent) {
    ::Rprintf("%s", this->format(indent).c_str());
  }
};

class BCIObjClass {
public:
  const std::string cls_ = "BCIObjClass";
  BCIObjClass(const SEXP& config) {};
  virtual SEXP toR() { return R_NilValue; };
  virtual std::string format() { return ""; };
  virtual void print() {};
};

class BCIStateParser : BCIObjClass {
public:

  const std::string cls_ = "BCIStateParser";
  std::vector<BCIStateDef> definition;
  std::vector<std::uint64_t> data;

  BCIStateParser(const SEXP& config) : BCIObjClass(config) {
    PROTECT(config);
    cpp11::strings confNames = Rf_getAttrib(config, R_NamesSymbol);
    size_t nConfig = (size_t) XLENGTH(config);
    if( nConfig > 0 ) {
      definition.reserve( nConfig );
      for(size_t i = 0; i < nConfig; i++) {

        cpp11::integers tmp = cpp11::as_integers(VECTOR_ELT(config, i));
        if( tmp.size() < 4 ) {
          throw std::runtime_error("Each BCIStateDef must have 4 values.");
        }
        BCIStateDef def( confNames[i], tmp[0], tmp[2], tmp[3], tmp[1] );
        definition.push_back( def );

        // BCIStateDef(
        //   const std::string & name,
        //   const int & len,
        //   const int & byteLoc,
        //   const int & bitLoc,
        //   const std::uint64_t & defaultVal
        // )
      }
    }
    UNPROTECT(1);
  }

  virtual std::string format() {
    std::string re = "<BCI2000 State Definitions>\r\n";
    for(
      std::vector<BCIStateDef>::iterator iter = definition.begin();
      iter != definition.end();
      iter++
    ) {
      re += iter->format("  ");
    }
    return re;
  }

  virtual void print() {
    ::Rprintf("%s", this->format().c_str());
  };
  virtual ~BCIStateParser () {};
  void parse(const char* ptr) {
    for(std::vector<BCIStateDef>::iterator iter = this->definition.begin();
        iter != this->definition.end(); iter++)
    {
      this->data.push_back(iter->parse(ptr));
    }
  };

};

class BCIDataParser : BCIObjClass {
private:

  size_t getRowBytes() {
    size_t elemSize = 1;
    switch (this->dataFormat) {

      case 1: elemSize = 2; break;
      case 2: elemSize = 4; break;
      case 3: elemSize = 4; break;
      default: {
        throw std::runtime_error("Unknown BCI2000 data format");
      }
    };
    return this->nChannels * elemSize + this->stateBytes;
  }

public:

  const std::string cls_ = "BCIDataParser";
  size_t nChannels;

  size_t stateBytes;
  BCIStateParser* stateParser;

  // 1: int16   2: int32   3: float32
  std::string dataFormat_;
  int dataFormat;
  std::vector<double> data;

  BCIDataParser(const cpp11::list & config) : BCIObjClass(config) {
    this->nChannels = cpp11::as_integers(config["n_channels"])[0];

    // nbites / 8
    this->stateBytes = cpp11::as_integers(config["state_bytes"])[0];
    this->stateParser = new BCIStateParser(config["state_definitions"]);
    std::string dfmt = cpp11::strings(config["data_format"])[0];
    this->dataFormat_ = dfmt;
    if( dfmt.compare("int16") == 0 ) {
      this->dataFormat = 1;
    } else if( dfmt.compare("int32") == 0 ) {
      this->dataFormat = 2;
    } else if( dfmt.compare("float32") == 0 ) {
      this->dataFormat = 3;
    } else {
      throw std::runtime_error("Unknown BCI2000 data format: " + dfmt);
    }
  };

  virtual ~BCIDataParser() {
    if( this->stateParser ) {
      delete this->stateParser;
    }
  }

  template <typename T>
  void parseInternal(char* ptr, size_t nBytes, bool reset = false) {
    size_t elemSize = sizeof( T );
    size_t rowBytes = this->nChannels * elemSize + this->stateBytes;
    size_t nSamples = nBytes / rowBytes;

    if( reset ) {
      data.clear();
    }
    if( !nSamples ) { return; }
    data.reserve( data.size() + nSamples * nChannels );
    // ::Rprintf("Current data size: %llu", data.size());

    size_t ii = 0, jj = 0;
    T* ptrData = (T*) ptr;
    for( ii = 0; ii < nSamples; ii++, ptr += rowBytes ) {

      ptrData = (T*) ptr;
      for( jj = 0 ; jj < this->nChannels; jj++, ptrData++ ) {
        data.push_back( *ptrData );
      }
      this->stateParser->parse( ptr + this->nChannels * elemSize );
    }

  }

  void parseSexp(const SEXP& x, bool reset = false) {
    if( TYPEOF(x) != RAWSXP ) {
      throw std::runtime_error("Input must be raw object to parse the BCI2000 data");
    }
    size_t xlen = XLENGTH(x);
    char* ptr = (char*) RAW(x);

    switch (this->dataFormat) {

    case 1: parseInternal<std::int16_t>(ptr, xlen, reset); break;
    case 2: parseInternal<std::int32_t>(ptr, xlen, reset); break;
    case 3: parseInternal<std::float_t>(ptr, xlen, reset); break;
    default: {
      throw std::runtime_error("Unknown BCI2000 data format");
    }

    };

  }

  virtual std::string format() {
    std::string re = this->stateParser->format();
    return (re + "<BCI2000 Data Definitions>" +
            "\r\n  Data Type: " + this->dataFormat_ +
            "\r\n  # of Channels: " + std::to_string(this->nChannels) +
            "\r\n  Each Sample Contains: " + std::to_string(this->getRowBytes()) + " Bytes" +
            "\r\n  # of Samples read: " + std::to_string(this->data.size() / this->nChannels) +
            "\r\n");
  }

  virtual void print() {
    ::Rprintf("%s", this->format().c_str());
  }

  virtual SEXP toR() {
    SEXP data_ = PROTECT(cpp11::as_sexp(this->data));
    SEXP data_dm = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(data_dm)[0] = (int) this->nChannels;
    INTEGER(data_dm)[1] = (int) (this->data.size() / this->nChannels);
    Rf_setAttrib(data_, R_DimSymbol, data_dm);

    SEXP states_ = PROTECT(cpp11::as_sexp(this->stateParser->data));
    SEXP states_dm = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(states_dm)[0] = (int) (this->stateParser->data.size() / INTEGER(data_dm)[1]);
    INTEGER(states_dm)[1] = INTEGER(data_dm)[1];

    Rf_setAttrib(states_, R_DimSymbol, states_dm);

    SEXP re_names = cpp11::writable::list({
      "data"_nm = data_,
      "states"_nm = states_
    });
    UNPROTECT(4); // states_dm, states_, data_dm, data_
    return re_names;
  }
};



[[cpp11::register]]
SEXP createBCIObject(const std::string & cls, const SEXP & config) {
  if( cls.compare("BCIStateParser") == 0 ) {
    SEXP re = PROTECT(bciObjCreate<BCIStateParser>(config));
    UNPROTECT(1);
    return re;
  }
  if( cls.compare("BCIDataParser") == 0 ) {
    SEXP re = PROTECT(bciObjCreate<BCIDataParser>(config));
    UNPROTECT(1);
    return re;
  }
  return R_NilValue;
}

[[cpp11::register]]
void printBCIObject(const SEXP & s) {
  BCIObjClass* sp = getBCIObjPointer<BCIObjClass>(s, true);
  sp->print();
}

[[cpp11::register]]
std::string formatBCIObject(const SEXP & s) {
  BCIObjClass* sp = getBCIObjPointer<BCIObjClass>(s, true);
  return sp->format();
}

[[cpp11::register]]
SEXP maturalizeBCIObject(const SEXP & s) {
  BCIObjClass* sp = getBCIObjPointer<BCIObjClass>(s, true);
  return sp->toR();
}

[[cpp11::register]]
void parseBCIDataRaw(const SEXP & s, const SEXP& x, bool reset = false) {
  BCIDataParser* sp = getBCIObjPointer<BCIDataParser>(s, true);
  sp->parseSexp(x, reset);
}
