#include <fstream>
#include <vector>
#include <cpp11.hpp>
#include "common.h"
using namespace cpp11;
namespace writable = cpp11::writable;

// A is uint64_t or uint32_t (timestamp)
// B is uint32_t indicating number of data points
// C is int16_t - data
template <typename A, typename B, typename C>
SEXP readNSxDataPacket(const std::string& filePath,
                         const size_t& nBytes,
                         const double& sampleRate,
                         const int& nChannels,
                         const size_t& skipBytes = 0,
                         const double& slope = 1.0,
                         const double& intercept = 0.0
) {

    try{

        // specs for NSx 3.0: 13bytes = 1byte 0x01 (uint8) + type (uint64 or 32) + # data points (uint32)
        const int typeSize = 1;
        const int timeSize = sizeof( A );
        // const int dpSize = 4;  // uint32 indicating number of data points (x channels x elemSize)
        const int overheadSize = typeSize + timeSize + sizeof( B );
        const size_t bufferSize = 4096;
        const double timeInterval = 1.0 / sampleRate; // time differences
        const size_t elemSize = sizeof( C ); // data element is stored in int16

        std::vector<double> timeStamps;
        std::vector<double> signals;

        std::ifstream input( filePath, std::ios::binary );

        // buffer
        SEXP buf = PROTECT( Rf_allocVector( REALSXP, bufferSize / sizeof( double ) ));   // bufferSize x 1B
        char* ptrChrBuf = (char *) REAL(buf);
        C* ptrInt16Buf0 = (C*) REAL(buf);
        C* ptrInt16Buf1 = ptrInt16Buf0;

        // temporary variables
        A currentTime_ = 0;
        double currentTime = 0.0;
        B nPoints = 0;
        size_t nPointsInBytes = 0;
        size_t readSize = 0;
        size_t readElementSize = 0;
        int count = 0;
        size_t remainBytes = nBytes;

        if( skipBytes > 0 ) {
            for(size_t skipRead = 0; skipRead < skipBytes; skipRead += bufferSize ) {
                // ::Rprintf("Skipping remain: %lld \n", (skipBytes-skipRead));
                input.read(ptrChrBuf, (skipBytes-skipRead) > bufferSize ? bufferSize : (skipBytes-skipRead) );
            }
            cpp11::check_user_interrupt();
        }

        // read
        while(remainBytes > 0) {
            cpp11::check_user_interrupt();

            input.read(ptrChrBuf, overheadSize);

            if( ptrChrBuf[0] - 1 != 0 ) {
                throw std::runtime_error("Data package identifier is not 0x01.");
            }

            remainBytes -= overheadSize;

            readnsx::convertBuffer(&currentTime_, ptrChrBuf + typeSize, 1);
            currentTime = ((double) currentTime_) / 1000000000.0;
            // ::Rprintf("Package Type: %d, time: %lld\n", ptrChrBuf[0], currentTime_);

            readnsx::convertBuffer(&nPoints, ptrChrBuf + (typeSize + timeSize), 1);
            // ::Rprintf("  Reading... %lld x %lld x %lld. Current ntime=%lld, ndata=%lld\n", nPoints, nChannels, elemSize, timeStamps.size(), signals.size());
            nPointsInBytes = (size_t) ( nPoints *elemSize * nChannels );

            while( nPointsInBytes > 0 && remainBytes > 0 ) {
                readSize = nPointsInBytes > bufferSize ? bufferSize : nPointsInBytes;
                if( readSize > remainBytes ) {
                    readSize = remainBytes;
                }
                input.read(ptrChrBuf, readSize);
                nPointsInBytes -= readSize;
                remainBytes -= readSize;
                readElementSize = readSize / elemSize;

                ptrInt16Buf1 = ptrInt16Buf0;
                for(; readElementSize > 0; readElementSize--, ptrInt16Buf1++ ) {
                    signals.push_back( ((double) *ptrInt16Buf1) * slope + intercept );
                    count++;
                    if( count >= nChannels ) {
                        timeStamps.push_back(currentTime);
                        currentTime += timeInterval;
                        count = 0;
                    }
                }
            }
        }
        UNPROTECT(1); // buf
        return writable::list({
            "timestamps"_nm = timeStamps,
                "data"_nm = signals
        });


    } catch (const std::exception& e) {
        ::Rf_error("%s", e.what());
    } catch(...) {
        ::Rf_error("Unknown error during reading the NSx file.");
    }

    return R_NilValue;
}


[[cpp11::register]]
SEXP readNSxDataPacket30(const std::string& filePath,
           const uint32_t& nBytes,
           const double& sampleRate,
           const int& nChannels,
           const uint32_t& skipBytes = 0,
           const double& slope = 1.0,
           const double& intercept = 0.0
) {
    return readNSxDataPacket<uint64_t, uint32_t, int16_t>(
            filePath, nBytes, sampleRate, nChannels, skipBytes,
            slope, intercept);
}

[[cpp11::register]]
SEXP readNSxDataPacket2x(const std::string& filePath,
                         const uint32_t& nBytes,
                         const double& sampleRate,
                         const int& nChannels,
                         const uint32_t& skipBytes = 0,
                         const double& slope = 1.0,
                         const double& intercept = 0.0
) {
    return readNSxDataPacket<uint32_t, uint32_t, int16_t>(
            filePath, nBytes, sampleRate, nChannels, skipBytes,
            slope, intercept);
}

/** R
path = "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV032b/BLOCK011/NSP-PAV032b_Datafile_010.ns3"
 readNSxDataPacket(path, 200, list(data_header = list(type = "packet", size = 13L, specs = list(
 header = list(type = "uint8"), timestamp = list(type = "uint64"),
 number_of_data_points = list(type = "uint32")))), 1304)
*/
