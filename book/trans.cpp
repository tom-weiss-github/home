
// g++ -o trans -std=c++11 trans.cpp
#include <iostream>
#include <algorithm>

int main(int argc, char* argv[])
{
    std::string orig( "abcdefg" );
    std::string nice;

    nice.resize( orig.length() );
    std::transform( orig.begin(), orig.end(),
                    nice.begin(),
                    []( char c )
                    {
                        if( 'a' == c ){ return 'x'; }
                        return c;
                    });

    std::cout << "orig=" << orig << std::endl;
    std::cout << "nice=" << nice << std::endl;
}
