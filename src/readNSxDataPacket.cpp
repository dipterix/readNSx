#include <fstream>
#include <vector>
#include <cpp11.hpp>
#include "common.h"
using namespace cpp11;
namespace writable = cpp11::writable;

// =============================================================================
// Scan NSx file to get packet information without reading data
// This allows pre-allocating HDF5 files and reading data in chunks
// =============================================================================

// A is uint64_t or uint32_t (timestamp)
// B is uint32_t indicating number of data points
// C is the data element type (e.g., int16_t)
template <typename A, typename B, typename C>
SEXP scanNSxPackets(const std::string& filePath,
                    const size_t& nBytes,
                    const int& nChannels,
                    const size_t& skipBytes = 0) {
    try {
        const int typeSize = 1;
        const int timeSize = sizeof(A);
        const int overheadSize = typeSize + timeSize + sizeof(B);
        const size_t elemSize = sizeof(C);

        std::vector<double> timestamps;     // packet start times (nanoseconds -> seconds)
        std::vector<double> nDataPoints;    // number of data points per packet
        std::vector<double> byteOffsets;    // byte offset of data (after header) from file start

        std::ifstream input(filePath, std::ios::binary);
        if (!input.is_open()) {
            throw std::runtime_error("Cannot open file for reading.");
        }

        // Skip header bytes
        input.seekg(skipBytes, std::ios::beg);

        size_t remainBytes = nBytes;
        size_t currentOffset = skipBytes;

        char headerBuf[24];  // max overhead size for NSx 3.0

        while (remainBytes >= (size_t)overheadSize) {
            cpp11::check_user_interrupt();

            input.read(headerBuf, overheadSize);
            if (!input) {
                break;
            }

            if (headerBuf[0] != 1) {
                // Invalid packet header
                break;
            }

            remainBytes -= overheadSize;
            currentOffset += overheadSize;

            // Read timestamp
            A currentTime_ = 0;
            readnsx::convertBuffer(&currentTime_, headerBuf + typeSize, 1);
            double currentTime = ((double)currentTime_) / 1000000000.0;

            // Read number of data points
            B nPoints = 0;
            readnsx::convertBuffer(&nPoints, headerBuf + (typeSize + timeSize), 1);

            size_t nPointsInBytes = (size_t)(nPoints) * elemSize * nChannels;

            timestamps.push_back(currentTime);
            nDataPoints.push_back((double)nPoints);
            byteOffsets.push_back((double)currentOffset);

            // Skip the data section
            if (nPointsInBytes > remainBytes) {
                nPointsInBytes = remainBytes;
            }
            input.seekg(nPointsInBytes, std::ios::cur);
            remainBytes -= nPointsInBytes;
            currentOffset += nPointsInBytes;
        }

        return writable::list({
            "timestamps"_nm = timestamps,
            "n_data_points"_nm = nDataPoints,
            "byte_offsets"_nm = byteOffsets,
            "n_packets"_nm = (int)timestamps.size()
        });

    } catch (const std::exception& e) {
        ::Rf_error("%s", e.what());
    } catch (...) {
        ::Rf_error("Unknown error during scanning NSx file.");
    }
    return R_NilValue;
}

[[cpp11::register]]
SEXP scanNSxPackets30(const std::string& filePath,
                      const size_t& nBytes,
                      const int& nChannels,
                      const size_t& skipBytes) {
    return scanNSxPackets<uint64_t, uint32_t, int16_t>(filePath, nBytes, nChannels, skipBytes);
}

[[cpp11::register]]
SEXP scanNSxPackets2x(const std::string& filePath,
                      const size_t& nBytes,
                      const int& nChannels,
                      const size_t& skipBytes) {
    return scanNSxPackets<uint32_t, uint32_t, int16_t>(filePath, nBytes, nChannels, skipBytes);
}

// =============================================================================
// Read a single packet's data given byte offset and number of points
// Returns data as double vector (channel x time, column-major)
// C is the data element type (e.g., int16_t)
// sampleOffset: number of time points to skip from the start of the packet (0-based)
// sampleCount: number of time points to read (-1 means read all remaining)
// =============================================================================

template <typename C>
SEXP readNSxPacketDataImpl(const std::string& filePath,
                           const size_t& byteOffset,
                           const int& nDataPoints,
                           const int& nChannels,
                           const cpp11::doubles& slope,
                           const cpp11::doubles& intercept,
                           const int& sampleOffset = 0,
                           const int& sampleCount = -1) {
    try {
        const size_t elemSize = sizeof(C);
        const size_t bufferSize = 4096;

        // Validate sampleOffset
        if (sampleOffset < 0 || sampleOffset >= nDataPoints) {
            throw std::runtime_error("sampleOffset out of range.");
        }

        // Calculate actual number of samples to read
        int actualSampleCount = sampleCount;
        if (actualSampleCount < 0 || (sampleOffset + actualSampleCount) > nDataPoints) {
            actualSampleCount = nDataPoints - sampleOffset;
        }

        if (actualSampleCount <= 0) {
            // Return empty matrix
            writable::doubles result(static_cast<R_xlen_t>(0));
            result.attr("dim") = writable::integers({nChannels, 0});
            return result;
        }

        // Calculate byte offset to skip within packet data section
        // byteOffset already points to the start of the data section (after packet header)
        // Each time point has nChannels elements of elemSize bytes
        const size_t skipBytes = static_cast<size_t>(sampleOffset) * nChannels * elemSize;
        const size_t actualByteOffset = byteOffset + skipBytes;
        const size_t totalBytes = static_cast<size_t>(actualSampleCount) * nChannels * elemSize;

        std::ifstream input(filePath, std::ios::binary);
        if (!input.is_open()) {
            throw std::runtime_error("Cannot open file for reading.");
        }

        input.seekg(actualByteOffset, std::ios::beg);

        // Allocate output: nChannels x actualSampleCount (column-major for R)
        writable::doubles result(static_cast<R_xlen_t>(nChannels) * actualSampleCount);

        // Buffer for reading
        std::vector<char> rawBuf(bufferSize);
        C* ptrData = reinterpret_cast<C*>(rawBuf.data());

        size_t remainBytes = totalBytes;
        int timeIdx = 0;
        int chanIdx = 0;

        while (remainBytes > 0) {
            cpp11::check_user_interrupt();

            size_t readSize = std::min(remainBytes, bufferSize);
            input.read(rawBuf.data(), readSize);
            if (!input) {
                throw std::runtime_error("Unexpected end of file.");
            }

            remainBytes -= readSize;
            size_t nElements = readSize / elemSize;

            for (size_t i = 0; i < nElements; i++) {
                // Column-major: result[chanIdx + timeIdx * nChannels]
                R_xlen_t idx = static_cast<R_xlen_t>(chanIdx) + static_cast<R_xlen_t>(timeIdx) * nChannels;
                result[idx] = ((double)ptrData[i]) * slope[chanIdx] + intercept[chanIdx];

                chanIdx++;
                if (chanIdx >= nChannels) {
                    chanIdx = 0;
                    timeIdx++;
                }
            }
        }

        result.attr("dim") = writable::integers({nChannels, actualSampleCount});
        return result;

    } catch (const std::exception& e) {
        ::Rf_error("%s", e.what());
    } catch (...) {
        ::Rf_error("Unknown error during reading NSx packet data.");
    }
    return R_NilValue;
}

[[cpp11::register]]
SEXP readNSxPacketData(const std::string& filePath,
                       const size_t& byteOffset,
                       const int& nDataPoints,
                       const int& nChannels,
                       const cpp11::doubles& slope,
                       const cpp11::doubles& intercept,
                       const int& sampleOffset = 0,
                       const int& sampleCount = -1) {
    return readNSxPacketDataImpl<int16_t>(filePath, byteOffset, nDataPoints, nChannels,
                                          slope, intercept, sampleOffset, sampleCount);
}


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
