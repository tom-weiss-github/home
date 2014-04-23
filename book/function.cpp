#include <iostream>
#include <functional>
// comment once

struct Caller
{
    typedef std::function<void(int, std::string)> CB;

    Caller(CB cb)
       : m_cb(cb)
    {}

    void Trigger()
    {
        m_cb(1, "trigger");
    }

    CB m_cb;
};

struct Callback
{
    void print(int i, std::string s)
    {
        std::cout << "i=" << i << " s=" << s << std::endl;
    }
};

int main(int /*argc*/, char** /*argv[]*/)
{
    Callback cb;
    Caller::CB funcCb = std::bind(&Callback::print, // member function
                                  &cb,               // object instance
                                  std::placeholders::_1, std::placeholders::_2);
    Caller caller(funcCb);
    caller.Trigger();

    return 0;
}
