class RLE {
  static encode(s) {
    var output = []
    for (c in s) {
      if (output.count == 0 || output[-1][0] != c) {
        output.add([c, 1])
      } else {
        output[-1][1] = output[-1][1] + 1
      }
    }

    return output.map {|c|
      if (c[1] == 1) {
        return c[0]
      } else {
        return "%(c[1])%(c[0])"
      }
    }.join()
  }

  static decode(s) {
    var input = []
    for (c in s) {
      // Note: Num.fromString(" ") returns 0 for some reason...
      var n = c != " " ? Num.fromString(c) : null
      if (n != null) {
        if (input.count == 0 || input[-1].count == 2) {
          input.add([n])
        } else {
          input[-1][0] = (input[-1][0] * 10) + n
        }
      } else if (input.count != 0 && input[-1].count == 1) {
        input[-1].add(c)
      } else {
        input.add([1, c])
      }
    }

    return input.map {|c| c[1] * c[0] }.join()
  }
}
