#include <Rcpp.h>
#include <chrono>
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <map>

#define duration(a) std::chrono::duration_cast<std::chrono::nanoseconds>(a).count()
#define now() std::chrono::high_resolution_clock::now()

typedef std::map<std::pair<std::string, int>, std::chrono::high_resolution_clock::time_point> TimesMap;

namespace Rcpp
{
  class Clock
  {
  private:
    std::vector<std::chrono::high_resolution_clock::time_point> tick_times;
    std::vector<std::chrono::high_resolution_clock::time_point> tock_times;
    std::vector<std::string> tick_names;
    std::vector<std::string> tock_names;
    std::vector<double> timers;
    std::vector<std::string> tickers;

  public:
    // start a timer
    void tick(std::string name)
    {
      tick_names.push_back(name);
      tick_times.push_back(now());
    }

    // stop a timer
    void tock(std::string name)
    {
      tock_names.push_back(name);
      tock_times.push_back(now());
    }

    // calculate timer durations
    void stop(std::string var_name)
    {
      std::pair<TimesMap::iterator, bool> insertTime;
      std::pair<std::string, int> key;

      /* Sort ticks and tocks by tick_name and tock_name */

      TimesMap tickmap;
      for (std::size_t i = 0; i < tick_names.size(); ++i)
      {
        key.first = tick_names[i];
        key.second = i;
        insertTime = tickmap.insert(
            std::pair<std::pair<std::string, int>, std::chrono::high_resolution_clock::time_point>(key, tick_times[i]));
      }

      TimesMap tockmap;
      for (std::size_t i = 0; i < tock_names.size(); ++i)
      {
        key.first = tock_names[i];
        key.second = i;
        insertTime = tockmap.insert(
            std::pair<std::pair<std::string, int>, std::chrono::high_resolution_clock::time_point>(key, tock_times[i]));
      }

      std::vector<std::string> keys;
      keys.reserve(tickmap.size());
      std::vector<std::chrono::high_resolution_clock::time_point> ticks;
      ticks.reserve(tickmap.size());
      for (auto kv : tickmap)
      {
        keys.push_back(kv.first.first);
        ticks.push_back(kv.second);
      }

      std::vector<std::chrono::high_resolution_clock::time_point> tocks;
      tocks.reserve(tockmap.size());
      for (auto kv : tockmap)
      {
        tocks.push_back(kv.second);
      }

      for (std::size_t i = 0; i < ticks.size(); ++i)
      {
        tickers.push_back(keys[i]);
        timers.push_back(duration(tocks[i] - ticks[i]));
      }
      DataFrame df = DataFrame::create(Named("ticker") = tickers, Named("timer") = timers);
      df.attr("class") = "RcppClock";
      Environment env = Environment::global_env();
      env[var_name] = df;
    }
  };
}