#include <iostream>
// comment 2

struct X
{
    int a;
    int b;
};

int main(int , char** )
{
    std::cout << "Crashing..." << std::endl;
    X* ptr = 0;
    std::cout << ptr->a << std::endl;

    return(0);
}
