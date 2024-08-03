#if !defined(MEETUP_H)
#define MEETUP_H

#include <boost/date_time/gregorian/gregorian.hpp>
namespace meetup {
typedef boost::gregorian::nth_day_of_the_week_in_month nth_dow;
typedef boost::gregorian::first_day_of_the_week_after fdowa;
using wday = boost::gregorian::greg_weekday::weekday_enum;
using boost::gregorian::date;
struct scheduler {
  date::month_type month;
  int year;

private:
  date get_nth(nth_dow::week_num nth, wday day) const;
  date first(wday day) const;
  date second(wday day) const;
  date third(wday day) const;
  date fourth(wday day) const;
  date fifth(wday day) const;
  date last(wday day) const;

public:
  date first_monday() const;
  date second_monday() const;
  date third_monday() const;
  date fourth_monday() const;
  date fifth_monday() const;
  date last_monday() const;
  date first_tuesday() const;
  date second_tuesday() const;
  date third_tuesday() const;
  date fourth_tuesday() const;
  date fifth_tuesday() const;
  date last_tuesday() const;
  date first_wednesday() const;
  date second_wednesday() const;
  date third_wednesday() const;
  date fourth_wednesday() const;
  date fifth_wednesday() const;
  date last_wednesday() const;
  date first_thursday() const;
  date second_thursday() const;
  date third_thursday() const;
  date fourth_thursday() const;
  date fifth_thursday() const;
  date last_thursday() const;
  date first_friday() const;
  date second_friday() const;
  date third_friday() const;
  date fourth_friday() const;
  date fifth_friday() const;
  date last_friday() const;
  date first_saturday() const;
  date second_saturday() const;
  date third_saturday() const;
  date fourth_saturday() const;
  date fifth_saturday() const;
  date last_saturday() const;
  date first_sunday() const;
  date second_sunday() const;
  date third_sunday() const;
  date fourth_sunday() const;
  date fifth_sunday() const;
  date last_sunday() const;
  date monteenth() const;
  date tuesteenth() const;
  date wednesteenth() const;
  date thursteenth() const;
  date friteenth() const;
  date saturteenth() const;
  date sunteenth() const;
};

} // namespace meetup

#endif // MEETUP_H