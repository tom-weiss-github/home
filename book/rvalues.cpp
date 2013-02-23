#include <iostream>
#include <random>

struct HasMove
{
    HasMove()
       : m_data( new int[1000] )
    {
        std::random_device generator;
        std::uniform_int_distribution<int> distribution(1,100);
        m_data[0] = distribution(generator);
    }

    ~HasMove()
    {
        delete [] m_data;
    }

    HasMove( const HasMove& other )
       : m_data( new int[1000] )
    {
        std::cout << __FUNCTION__ << " copy" << std::endl;
        std::copy( other.m_data, other.m_data + 1000, m_data );
    }

    HasMove( HasMove&& other )
       : m_data( other.m_data )
    {
        std::cout << __FUNCTION__ << " move" << std::endl;
        other.m_data = nullptr;
    }

    void MoveFn( HasMove&& other )
    {
        std::cout << __FUNCTION__ << " " << other.m_data << std::endl;
    }

    std::string ToString()
    {
        if( nullptr == m_data )
        {
            return( "NULL" );
        }
        return( std::to_string(m_data[0]) );
    }

    int* m_data;
};

struct NoMove
{
    NoMove()
       : m_data( new int[1000] )
    {
        std::random_device generator;
        std::uniform_int_distribution<int> distribution(1,100);
        m_data[0] = distribution(generator);
    }

    ~NoMove()
    {
        delete [] m_data;
    }

    NoMove( const NoMove& other )
       : m_data( new int[1000] )
    {
        std::cout << __FUNCTION__ << " copy" << std::endl;
        std::copy( other.m_data, other.m_data + 1000, m_data );
    }

    std::string ToString()
    {
        if( nullptr == m_data )
        {
            return( "NULL" );
        }
        return( std::to_string(m_data[0]) );
    }

    int* m_data;
};

template<typename T>
void fn( T&& t )
{
    t++;
    std::cout << "t=" << t << std::endl;
}

template<typename T>
void ad( T&& t )
{
    T tmp = t + 1.0;
    std::cout << "tmp=" << tmp << std::endl;
}

int main(int /*argc*/, char** /*argv*/)
{
    HasMove a;
    HasMove b = std::move( a );
    //HasMove c = a;  Will crash.
    HasMove c = b;

    HasMove m;
    HasMove n;
    // m.MoveFn( n ); This will not compile.
    m.MoveFn( std::move( n ) );

    // std::cout << a.ToString() << std::endl;
    // std::cout << b.ToString() << std::endl;
    // std::cout << c.ToString() << std::endl;

    NoMove nm1;
    NoMove nm2 = std::move( nm1 );

    fn(42);

    int i = 100;
    fn(i);
    fn(i);

    int j = 100;
    fn( j );
    std::cout << "j=" << j << std::endl;

    j = 100;
    fn( std::move( j ) );
    std::cout << "j=" << j << std::endl;

    j = 100;
    fn( static_cast<int&&>(j) );
    std::cout << "j=" << j << std::endl;

    ad( 5 );
    ad( static_cast<float&&>(5.1) );
    ad( static_cast<int&&>(5.1) );
    ad(5.1);
}
