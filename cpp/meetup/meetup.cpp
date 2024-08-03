#include "meetup.h"

namespace meetup {

date scheduler::get_nth(nth_dow::week_num nth, wday day) const {
  nth_dow ndm(nth, day, month);
  return ndm.get_date(year);
}
date scheduler::first(wday day) const { return get_nth(nth_dow::first, day); }
date scheduler::second(wday day) const { return get_nth(nth_dow::second, day); }
date scheduler::third(wday day) const { return get_nth(nth_dow::third, day); }
date scheduler::fourth(wday day) const {
  return get_nth(nth_dow::fourth, day);
};
date scheduler::fifth(wday day) const { return get_nth(nth_dow::fifth, day); }
date scheduler::last(wday day) const { return fifth(day); }
date scheduler::first_monday() const { return first(wday::Monday); }
date scheduler::second_monday() const { return second(wday::Monday); }
date scheduler::third_monday() const { return third(wday::Monday); }
date scheduler::fourth_monday() const { return fourth(wday::Monday); }
date scheduler::fifth_monday() const { return fifth(wday::Monday); }
date scheduler::last_monday() const { return last(wday::Monday); }

date scheduler::first_tuesday() const { return first(wday::Tuesday); }
date scheduler::second_tuesday() const { return second(wday::Tuesday); }
date scheduler::third_tuesday() const { return third(wday::Tuesday); }
date scheduler::fourth_tuesday() const { return fourth(wday::Tuesday); }
date scheduler::fifth_tuesday() const { return fifth(wday::Tuesday); }
date scheduler::last_tuesday() const { return last(wday::Tuesday); }

date scheduler::first_wednesday() const { return first(wday::Wednesday); }
date scheduler::second_wednesday() const { return second(wday::Wednesday); }
date scheduler::third_wednesday() const { return third(wday::Wednesday); }
date scheduler::fourth_wednesday() const { return fourth(wday::Wednesday); }
date scheduler::fifth_wednesday() const { return fifth(wday::Wednesday); }
date scheduler::last_wednesday() const { return last(wday::Wednesday); }

date scheduler::first_thursday() const { return first(wday::Thursday); }
date scheduler::second_thursday() const { return second(wday::Thursday); }
date scheduler::third_thursday() const { return third(wday::Thursday); }
date scheduler::fourth_thursday() const { return fourth(wday::Thursday); }
date scheduler::fifth_thursday() const { return fifth(wday::Thursday); }
date scheduler::last_thursday() const { return last(wday::Thursday); }

date scheduler::first_friday() const { return first(wday::Friday); }
date scheduler::second_friday() const { return second(wday::Friday); }
date scheduler::third_friday() const { return third(wday::Friday); }
date scheduler::fourth_friday() const { return fourth(wday::Friday); }
date scheduler::fifth_friday() const { return fifth(wday::Friday); }
date scheduler::last_friday() const { return last(wday::Friday); }

date scheduler::first_saturday() const { return first(wday::Saturday); }
date scheduler::second_saturday() const { return second(wday::Saturday); }
date scheduler::third_saturday() const { return third(wday::Saturday); }
date scheduler::fourth_saturday() const { return fourth(wday::Saturday); }
date scheduler::fifth_saturday() const { return fifth(wday::Saturday); }
date scheduler::last_saturday() const { return last(wday::Saturday); }

date scheduler::first_sunday() const { return first(wday::Sunday); }
date scheduler::second_sunday() const { return second(wday::Sunday); }
date scheduler::third_sunday() const { return third(wday::Sunday); }
date scheduler::fourth_sunday() const { return fourth(wday::Sunday); }
date scheduler::fifth_sunday() const { return fifth(wday::Sunday); }
date scheduler::last_sunday() const { return last(wday::Sunday); }

date scheduler::monteenth() const {
  fdowa fdaf(wday::Monday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::tuesteenth() const {
  fdowa fdaf(wday::Tuesday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::wednesteenth() const {
  fdowa fdaf(wday::Wednesday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::thursteenth() const {
  fdowa fdaf(wday::Thursday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::friteenth() const {
  fdowa fdaf(wday::Friday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::saturteenth() const {
  fdowa fdaf(wday::Saturday);
  return fdaf.get_date(date(year, month, 12));
}
date scheduler::sunteenth() const {
  fdowa fdaf(wday::Sunday);
  return fdaf.get_date(date(year, month, 12));
}

} // namespace meetup
