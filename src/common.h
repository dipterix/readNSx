#include <cstring>

namespace readnsx {

template <typename A>
void convertBuffer(A* ptr, char* bufptr, const int& n) {

    int esize = sizeof( A ) / sizeof( char );
    for(int i = 0; i < n; i++, ptr++, bufptr += esize ) {
        std::memcpy(ptr, bufptr, esize);
    }
}


};
