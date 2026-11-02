#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <cpp11.hpp>
#include "common.h"
using namespace cpp11;
namespace writable = cpp11::writable;

// =============================================================================
// Scan an NSx file into partitions without reading the data values
//
// Terms follow the NSx specification (LB-0023): a data block is a 0x01 byte, a
// timestamp, the number of data points, and the data points; a data point
// holds one int16 per channel.
//
// A data block with more than one data point is a partition by itself: a new
// data block starts after every pause. Runs of consecutive one-point data
// blocks (recordings with PTP timestamps) are split only at pauses, found the
// way Blackrock's NPMK `openNSx` does: the run is checked in frames of
// `packetsPerFrame` data blocks (neighbouring frames share their boundary
// block); a frame whose elapsed time exceeds what its data points imply by at
// least `maxTickMultiple - 1` sampling intervals is split wherever two
// consecutive timestamps are more than `maxTickMultiple` intervals apart.
// Clock jumps that cancel out within a frame, and backward jumps, never split.
//
// Besides one row per partition, the result has `bytes_unread` and
// `stop_reason` (empty when the whole data section was read), so that R can
// warn when a file ends early or has a corrupt data block header.
//
// Counts and file positions are 64-bit: a partition may hold more than 2^32
// data points and files may be larger than 4 GB.
// =============================================================================

namespace {

struct PartitionTable {
    std::vector<double> byteOffset;   // file position of the first data point values
    std::vector<double> stride;       // bytes from one data point to the next
    std::vector<double> nPoints;      // number of data points
    std::vector<double> startTime;    // first timestamp in seconds

    void add(uint64_t offset, uint64_t strideBytes, uint64_t n,
             uint64_t timestamp, double timestampResolution) {
        byteOffset.push_back((double) offset);
        stride.push_back((double) strideBytes);
        nPoints.push_back((double) n);
        startTime.push_back((double) timestamp / timestampResolution);
    }
};

// whole non-negative number that fits in 64 bits, from an R double
uint64_t as_count(const double& x, const char* what) {
    if (!R_FINITE(x) || x < 0 || x != std::floor(x) || x >= 18446744073709551616.0) {
        throw std::runtime_error(std::string(what) + " must be a non-negative whole number.");
    }
    return (uint64_t) x;
}

}

[[cpp11::register]]
SEXP scanNSxPartitions(const std::string& filePath,
                       const double& skipBytes,
                       const double& nBytes,
                       const int& nChannels,
                       const int& timestampBytes,
                       const double& timestampResolution,
                       const double& sampleRate,
                       const double& packetsPerFrame,
                       const double& maxTickMultiple) {
    if (nChannels <= 0) {
        throw std::runtime_error("nChannels must be positive.");
    }
    if (timestampBytes != 4 && timestampBytes != 8) {
        throw std::runtime_error("timestampBytes must be 4 or 8.");
    }
    if (!(timestampResolution > 0) || !(sampleRate > 0) || !(maxTickMultiple > 0)) {
        throw std::runtime_error("timestampResolution, sampleRate and maxTickMultiple must be positive.");
    }
    const uint64_t frameSize = as_count(packetsPerFrame, "packetsPerFrame");
    if (frameSize < 2) {
        throw std::runtime_error("packetsPerFrame must be at least 2.");
    }

    const uint64_t headerSize = 1 + timestampBytes + 4;
    const uint64_t pointSize = 2 * (uint64_t) nChannels;
    const uint64_t blockStride = headerSize + pointSize;  // one-point data blocks
    // thresholds computed in the same order as NPMK, so ties round the same way
    const double ticksPerPoint = timestampResolution / sampleRate;
    const double pauseTicks = maxTickMultiple * ticksPerPoint;
    const double extraTicks = pauseTicks - ticksPerPoint;

    // a larger stream buffer: one-point data blocks are read back to back
    std::vector<char> streamBuffer(1 << 20);
    std::ifstream input;
    input.rdbuf()->pubsetbuf(streamBuffer.data(), streamBuffer.size());
    input.open(filePath, std::ios::binary);
    if (!input.is_open()) {
        throw std::runtime_error("Cannot open file for reading.");
    }

    uint64_t position = as_count(skipBytes, "skipBytes");
    uint64_t remainBytes = as_count(nBytes, "nBytes");
    input.seekg((std::streamoff) position, std::ios::beg);

    PartitionTable table;

    // the current run of one-point data blocks; indices count data points
    // from the start of the run
    bool inRun = false;
    uint64_t runOffset = 0;       // file position of the run's first values
    uint64_t runCount = 0;        // data points in the run so far
    uint64_t partStart = 0;       // where the open partition starts
    uint64_t partTimestamp = 0;   // timestamp of that data point
    uint64_t frameStart = 0;      // run index of frame[0]
    uint64_t runPauses = 0;       // pauses found in the run so far
    std::vector<uint64_t> frame;  // timestamps of the current frame
    frame.reserve((size_t) std::min<uint64_t>(frameSize, 1 << 20));

    // A recording does not pause at most of its data points. If it seems to,
    // the timestamps do not tick at `timestampResolution` (a wrong header
    // value), and splitting would create one partition per data point
    auto checkPlausible = [&]() {
        const uint64_t steps = frameStart + frame.size() - 1;
        if (steps < 10 || runPauses * 2 <= steps) { return; }
        std::vector<double> diffs;
        for (size_t j = 0; j + 1 < frame.size(); j++) {
            diffs.push_back((double) (int64_t) (frame[j + 1] - frame[j]));
        }
        std::nth_element(diffs.begin(), diffs.begin() + diffs.size() / 2, diffs.end());
        throw std::runtime_error(
            "NSx file " + filePath + ": the time-stamps advance about " +
            std::to_string((long long) diffs[diffs.size() / 2]) +
            " ticks per data point, but time_resolution_timestamp = " +
            std::to_string((long long) timestampResolution) + " at " +
            std::to_string((long long) sampleRate) + " Hz implies " +
            std::to_string(ticksPerPoint) + ", so more than half of the data " +
            "points would start a new partition. The time-stamp resolution in " +
            "the file header looks wrong.");
    };

    auto checkFrame = [&]() {
        const size_t k = frame.size();
        if (k < 2) { return; }
        // unsigned difference reinterpreted as signed: a backward jump is
        // negative, not a huge positive number
        const double elapsed = (double) (int64_t) (frame[k - 1] - frame[0]);
        const double expected = (double) (k - 1) * ticksPerPoint;
        if (elapsed < expected + extraTicks) { return; }
        for (size_t j = 0; j + 1 < k; j++) {
            const double step = (double) (int64_t) (frame[j + 1] - frame[j]);
            if (step > pauseTicks) {
                const uint64_t last = frameStart + j;  // last data point before the pause
                table.add(runOffset + partStart * blockStride, blockStride,
                          last - partStart + 1, partTimestamp, timestampResolution);
                partStart = last + 1;
                partTimestamp = frame[j + 1];
                runPauses++;
            }
        }
        checkPlausible();
    };

    auto closeRun = [&]() {
        if (!inRun) { return; }
        checkFrame();
        if (runCount > partStart) {
            table.add(runOffset + partStart * blockStride, blockStride,
                      runCount - partStart, partTimestamp, timestampResolution);
        }
        inRun = false;
        frame.clear();
    };

    char header[13];  // 0x01, timestamp (4 or 8 bytes), number of data points
    uint64_t nBlocks = 0;
    std::string stopReason;  // why the scan ended before the end of the data

    while (remainBytes >= headerSize) {
        if ((++nBlocks & 0xFFFF) == 0) {
            cpp11::check_user_interrupt();
        }

        input.read(header, headerSize);
        if (!input) {
            stopReason = "the file could not be read to its end";
            break;
        }
        if (header[0] != 1) {
            stopReason = "a data block header does not start with 0x01";
            break;
        }
        position += headerSize;
        remainBytes -= headerSize;

        uint64_t timestamp = 0;
        if (timestampBytes == 8) {
            readnsx::convertBuffer(&timestamp, header + 1, 1);
        } else {
            uint32_t timestamp32 = 0;
            readnsx::convertBuffer(&timestamp32, header + 1, 1);
            timestamp = timestamp32;
        }
        uint32_t declared = 0;
        readnsx::convertBuffer(&declared, header + 1 + timestampBytes, 1);

        // a truncated last data block keeps only its complete data points
        const uint64_t available = remainBytes / pointSize;
        const uint64_t nPoints = std::min<uint64_t>(declared, available);

        if (declared == 1 && nPoints == 1) {
            // a one-point data block not adjacent to the run cannot share its
            // stride; does not happen in well-formed files
            if (inRun && position != runOffset + runCount * blockStride) {
                closeRun();
            }
            if (!inRun) {
                inRun = true;
                runOffset = position;
                runCount = 0;
                partStart = 0;
                partTimestamp = timestamp;
                frameStart = 0;
                runPauses = 0;
            }
            frame.push_back(timestamp);
            runCount++;
            if (frame.size() >= frameSize) {
                checkFrame();
                const uint64_t boundary = frame.back();
                frame.clear();
                frame.push_back(boundary);
                frameStart = runCount - 1;
            }
        } else {
            // multi-point and empty data blocks end a run: they change the
            // byte layout the run's stride relies on
            closeRun();
            if (nPoints > 0) {
                table.add(position, pointSize, nPoints, timestamp, timestampResolution);
            }
        }

        const uint64_t dataBytes = nPoints * pointSize;
        if (dataBytes <= streamBuffer.size()) {
            input.ignore((std::streamsize) dataBytes);
        } else {
            input.seekg((std::streamoff) dataBytes, std::ios::cur);
        }
        position += dataBytes;
        remainBytes -= dataBytes;

        if (nPoints < declared) {
            stopReason = "the last data block holds fewer data points than its header declares";
            break;
        }
    }
    if (stopReason.empty() && remainBytes > 0) {
        stopReason = "the file ends inside a data block header";
    }
    closeRun();

    return readnsx::makeNamedList(
        "byte_offset", cpp11::as_sexp(table.byteOffset),
        "stride", cpp11::as_sexp(table.stride),
        "n_points", cpp11::as_sexp(table.nPoints),
        "start_time", cpp11::as_sexp(table.startTime),
        "bytes_unread", cpp11::as_sexp((double) remainBytes),
        "stop_reason", cpp11::as_sexp(stopReason)
    );
}

// =============================================================================
// Read data points `firstPoint` to `firstPoint + nPoints - 1` of one partition
// Data point k of the partition starts `byteOffset + k * stride` bytes into the
// file (see `scanNSxPartitions`). Returns a nChannels x nPoints matrix of
// `value * slope + intercept`.
// =============================================================================

[[cpp11::register]]
SEXP readNSxPoints(const std::string& filePath,
                   const double& byteOffset,
                   const double& stride,
                   const int& nChannels,
                   const cpp11::doubles& slope,
                   const cpp11::doubles& intercept,
                   const double& firstPoint,
                   const double& nPoints) {
    if (nChannels <= 0) {
        throw std::runtime_error("nChannels must be positive.");
    }
    if (slope.size() < nChannels || intercept.size() < nChannels) {
        throw std::runtime_error("slope and intercept need one value per channel.");
    }
    const uint64_t pointSize = 2 * (uint64_t) nChannels;
    const uint64_t strideBytes = as_count(stride, "stride");
    if (strideBytes < pointSize) {
        throw std::runtime_error("stride is smaller than one data point.");
    }
    const uint64_t first = as_count(firstPoint, "firstPoint");
    const uint64_t count = as_count(nPoints, "nPoints");
    // the result is a matrix: its column count is limited to 2^31 - 1
    if (count > (uint64_t) INT_MAX) {
        throw std::runtime_error("nPoints is too large for one read; read in batches.");
    }
    const uint64_t start = as_count(byteOffset, "byteOffset") + first * strideBytes;

    std::vector<double> slopes(nChannels), intercepts(nChannels);
    for (int c = 0; c < nChannels; c++) {
        slopes[c] = slope[c];
        intercepts[c] = intercept[c];
    }

    writable::doubles result((R_xlen_t) nChannels * (R_xlen_t) count);
    double* out = REAL(result.data());

    if (count > 0) {
        std::ifstream input(filePath, std::ios::binary);
        if (!input.is_open()) {
            throw std::runtime_error("Cannot open file for reading.");
        }
        input.seekg((std::streamoff) start, std::ios::beg);

        // whole data points per read, about 4 MB at a time
        const uint64_t pointsPerRead = std::max<uint64_t>(1, (4 << 20) / strideBytes);
        std::vector<char> buffer((size_t) (std::min(pointsPerRead, count) * strideBytes));

        uint64_t done = 0;
        while (done < count) {
            cpp11::check_user_interrupt();

            const uint64_t m = std::min(pointsPerRead, count - done);
            // the last data point needs only its values, not the next header
            input.read(buffer.data(), (std::streamsize) ((m - 1) * strideBytes + pointSize));
            if (!input) {
                throw std::runtime_error("Unexpected end of file.");
            }
            for (uint64_t j = 0; j < m; j++) {
                const char* values = buffer.data() + j * strideBytes;
                double* column = out + (R_xlen_t) (done + j) * nChannels;
                for (int c = 0; c < nChannels; c++) {
                    int16_t value;
                    std::memcpy(&value, values + 2 * c, sizeof(int16_t));
                    column[c] = ((double) value) * slopes[c] + intercepts[c];
                }
            }
            done += m;
            if (done < count && strideBytes > pointSize) {
                input.ignore((std::streamsize) (strideBytes - pointSize));
            }
        }
    }

    result.attr("dim") = writable::integers({nChannels, (int) count});
    return result;
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
        return readnsx::makeNamedList(
            "timestamps", cpp11::as_sexp(timeStamps),
            "data", cpp11::as_sexp(signals)
        );

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
