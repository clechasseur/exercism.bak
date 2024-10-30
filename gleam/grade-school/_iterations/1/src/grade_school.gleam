import gleam/int
import gleam/list
import gleam/order
import gleam/string

pub type School {
  School(roster: List(#(String, Int)))
}

pub fn create() -> School {
  School([])
}

pub fn roster(school: School) -> List(String) {
  school.roster
  |> list.sort(fn (p1, p2) {
    int.compare(p1.1, p2.1)
    |> order.lazy_break_tie(fn () { string.compare(p1.0, p2.0) })
  })
  |> list.map(fn (p) { p.0 })
}

pub fn add(
  to school: School,
  student student: String,
  grade grade: Int,
) -> Result(School, Nil) {
  case list.any(school.roster, fn (p) { p.0 == student }) {
    False -> Ok(School([#(student, grade), ..school.roster]))
    True -> Error(Nil)
  }
}

pub fn grade(school: School, desired_grade: Int) -> List(String) {
  school.roster
  |> list.filter(fn (p) { p.1 == desired_grade })
  |> list.map(fn (p) { p.0 })
  |> list.sort(string.compare)
}
