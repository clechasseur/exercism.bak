class Flatten {
  static flatten(list) {
    if (!(list is Sequence)) {
      Fiber.abort("flatten only works on Sequences")
    }

    return list.reduce([]) {|acc, e|
      if (e is Sequence) {
        acc.addAll(flatten(e))
      } else if (e != null) {
        acc.add(e)
      }
      return acc
    }
  }
}
