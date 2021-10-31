#include <Rcpp.h>
#include <chrono>

typedef std::chrono::high_resolution_clock::time_point time_point;
#define duration(a) std::chrono::duration_cast<std::chrono::nanoseconds>(a).count()
#define now() std::chrono::high_resolution_clock::now()

namespace Rcpp {
  class Clock {
  private:
    std::vector<double> times;
    std::vector<std::string> names;
    time_point tick_time;
    bool is_tocked = true;
    
  public:
    void tick(const std::string name) {
      names.push_back(name);
      if (!is_tocked) {
        times.push_back(duration(now() - tick_time));
      } else is_tocked = false;
      tick_time = now();
    }
    
    void tock() {
      if (!is_tocked) {
        times.push_back(duration(now() - tick_time));
        is_tocked = true;
      }
    }
    
    void write(std::string name) {
      Environment env = Environment::global_env();
      NumericVector d = wrap(times);
      CharacterVector e = wrap(names);
      DataFrame df = DataFrame::create(Named("ticker") = names , _["timer"] = times );
      df.attr("class") = "RcppClock";
      env[name] = df;
    }
  };
}