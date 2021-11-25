#include <Rcpp.h>
#include <chrono>

namespace Rcpp {
  class Clock {
    private:
    std::vector<std::chrono::steady_clock::time_point> tick_times;
    std::vector<std::string> tick_names, durations_names;
    std::vector<double> durations;

    public:

    // start a timer
    void tick(std::string name) {
      tick_names.push_back(name);
      tick_times.resize(tick_times.size() + 1);
      tick_times.back() = std::chrono::steady_clock::now();
    }

    // stop a timer
    void tock(std::string name) {
      std::chrono::steady_clock::time_point tock_time = std::chrono::steady_clock::now();
      durations_names.push_back(name);
      for (size_t i = tick_names.size(); i > 0; --i) {
        if (tick_names[i - 1] == name) {
          durations.push_back(std::chrono::duration_cast<std::chrono::nanoseconds>(tock_time - tick_times[i - 1]).count());
          return;
        }
      }
      Rcpp::stop("Rcpp::Clock 'tock' with name of '" + name + "' did not match with a corresponding 'tick' name");
    }

    // calculate timer durations
    void stop(std::string var_name) {
      DataFrame df = DataFrame::create(Named("ticker") = durations_names, Named("timer") = durations);
      df.attr("class") = "RcppClock";
      Environment env = Environment::global_env();
      env[var_name] = df;
    }
  };
}