#include <Rcpp.h>
#include <chrono>

namespace Rcpp {
  class Clock {
  private:
    std::vector<std::chrono::steady_clock::time_point> tick_times, tock_times;
    std::vector<std::string> tick_names, tock_names, unique_tickers;

  public:
    // start a timer
    void tick(std::string name) {
      tick_names.push_back(name);
      tick_times.push_back(std::chrono::steady_clock::now());
    }

    // stop a timer
    void tock(std::string name) {
      tock_names.push_back(name);
      tock_times.push_back(std::chrono::steady_clock::now());
    }

    // calculate timer durations
    void stop(std::string var_name) {

      // create a timers vector holding runtimes for each tick/tock pair
      size_t num_ticks = tick_names.size();
      std::vector<double> timers(num_ticks);
      std::vector<std::string> tickers(num_ticks);

      // find unique ticker names
      for (size_t i = 0; i < num_ticks; ++i)
        if (std::find(unique_tickers.begin(), unique_tickers.end(), tick_names[i]) == unique_tickers.end())
          unique_tickers.push_back(tick_names[i]);

      // map all tick_names to tock_names and compute corresponding duration between tick_times and tock_times indices
      size_t i_timer = 0, i_tick = 0;
      for (auto ticker : unique_tickers) {
        for (size_t i = 0, j = 0; i < num_ticks; ++i) {
          if (tick_names[i] == ticker) {
            // find next tock_names/tock_times index corresponding to "ticker"
            while (tock_names[j] != ticker) ++j;
            tickers[i_tick] = ticker;
            timers[i_timer] = std::chrono::duration_cast<std::chrono::nanoseconds>(tock_times[j] - tick_times[i]).count();
            ++j;
            ++i_tick;
            ++i_timer;
          }
        }
      }
      DataFrame df = DataFrame::create(Named("ticker") = tickers, Named("timer") = timers);
      df.attr("class") = "RcppClock";
      Environment env = Environment::global_env();
      env[var_name] = df;
    }
  };
}