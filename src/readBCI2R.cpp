#include <stdexcept>
#include "readBCI.h"
using namespace cpp11;

// ---- BCIObjClass and R interface --------------------------------------------

[[cpp11::register]]
void printBCIObject(const SEXP & s) {
  try {
    readnsx::bci::printBCIObject(s);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `printBCIObject`.");
  }
}

[[cpp11::register]]
std::string formatBCIObject(const SEXP & s) {
  try {
    return readnsx::bci::formatBCIObject(s);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `formatBCIObject`.");
  }
}

[[cpp11::register]]
SEXP maturalizeBCIObject(const SEXP & s) {
  try {
    return readnsx::bci::maturalizeBCIObject(s);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `maturalizeBCIObject`.");
  }
}

[[cpp11::register]]
std::string bciStrDecode(const std::string& x, const std::string& nil = "NA") {
  try {
    return readnsx::bci::bciStrDecode(x, nil);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `bciStrDecode`.");
  }
}

[[cpp11::register]]
SEXP createBCIObject(const std::string & cls, const SEXP & config) {
  try {
    return readnsx::bci::createBCIObject(cls, config);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `createBCIObject`.");
  }
}

[[cpp11::register]]
void parseBCIDataRaw(const SEXP & s, const SEXP& x, bool reset = false) {
  try {
    readnsx::bci::parseBCIDataRaw(s, x, reset);
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `parseBCIDataRaw`.");
  }
}

[[cpp11::register]]
SEXP parseBCIParamDef(const std::string & statement) {
  try {
    readnsx::bci::BCIParamDef param(statement);
    return param.parse();
  } catch (const std::exception& e) {
    ::Rf_error("C++ Exception: %s\n", e.what());
  } catch (...) {
    ::Rf_error("%s\n", "Unknown C++ error in `parseBCIParamDef`.");
  }
}
