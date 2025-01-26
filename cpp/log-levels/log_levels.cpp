#include <sstream>
#include <string>
#include <string_view>

namespace log_line {

namespace {

constexpr std::string_view log_level_suffix{"]: "};
    
} // anonymous namespace
    
std::string message(std::string line) {
    return line.substr(line.find(log_level_suffix) + log_level_suffix.size());
}

std::string log_level(std::string line) {
    return line.substr(1, line.find(log_level_suffix) - 1);
}

std::string reformat(std::string line) {
    std::ostringstream oss;
    oss << message(line) << " (" << log_level(line) << ")";
    return oss.str();
}

} // namespace log_line
