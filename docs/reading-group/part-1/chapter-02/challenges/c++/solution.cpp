#include <cstdio>
#include <functional>
#include <iostream>
#include <map>

// 4. Which of these C++ functions are pure? Try to memoize them and
//    observe what happens when you call them multiple times: memoized and
//    not.

//    1. The factorial function from the example in the text.
//    2. .. code-block:: c++

//           std::getchar()

//    3. .. code-block:: c++

//           bool f() {
//               std::cout << "Hello!" << std::endl;
//               return true;
//           }

//    4. .. code-block:: c++

//           int f(int x)
//           {
//               static int y = 0;
//               y += x;
//               return y;
//           }

// A: None are pure! All behave differently under memoization.

template <typename Rslt, typename Arg=void>
class Memoize {
public:
    Memoize(std::function<Rslt(Arg)> func) :
        func_(func)
    {}

    Rslt operator()(Arg arg) {
        auto i = results_.find(arg);;
        if (i == results_.end()) {
            auto rslt = func_(arg);
            results_.insert(std::make_pair(arg, rslt));
            return rslt;
        }
        else {
            return i->second;
        }
    }

private:
    std::function<Rslt(Arg)> func_;
    std::map<Arg, Rslt> results_;
};

template <typename Rslt>
class Memoize<Rslt, void> {
public:
    Memoize(std::function<Rslt()> func) :
        func_(func)
    {}

    Rslt operator()() {
        if (!result_) {
            result_.reset(new Rslt(func_()));
        }
        return *result_;
    }

private:
    std::function<Rslt()> func_;
    std::unique_ptr<Rslt> result_;
};


auto gc = Memoize<int>(std::getchar);

bool f() {
    std::cout << "Hello!" << std::endl;
    return true;
}

int f2(int x)
{
    static int y = 0;
    y += x;
    return y;
}

auto mf = Memoize<bool>(f);
auto mf2 = Memoize<int, int>(f2);

int main(int argc, char** argv) {
    std::cout << gc() << "\n"
              << gc() << "\n";
    mf();
    mf();

    std::cout << mf2(42) << " "
              << mf2(42) << "\n";

    return 0;
}
