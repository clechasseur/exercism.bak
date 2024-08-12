func append<T>(_ list1: [T], _ list2: [T]) -> [T] {
    var result = list1

    for i in list2 {
        result.append(i)
    }

    return result
}

func concat<T>(_ lists: [T]...) -> [T] {
    var result: [T] = []

    for l in lists {
        for i in l {
            result.append(i)
        }
    }

    return result
}

func filter<T>(_ list: [T], _ f: (T) -> Bool) -> [T] {
    var result: [T] = []

    for i in list {
        if f(i) {
            result.append(i)
        }
    }

    return result
}

func length<T>(_ list: [T]) -> Int {
    var len = 0

    for _ in list {
        len += 1
    }

    return len
}

func map<T, U>(_ list: [T], f: (T) -> U) -> [U] {
    var result: [U] = []

    for i in list {
        result.append(f(i))
    }

    return result
}

func foldLeft<T, Acc>(_ list: [T], accumulated acc: Acc, combine f: (Acc, T) -> Acc) -> Acc {
    var result = acc

    for i in list {
        result = f(result, i)
    }

    return result
}

func foldRight<T, Acc>(_ list: [T], accumulated acc: Acc, combine f: (T, Acc) -> Acc) -> Acc {
    var result = acc

    for i in stride(from: length(list), to: 0, by: -1) {
        result = f(list[i - 1], result)
    }

    return result
}

func reverse<T>(_ list: [T]) -> [T] {
    var result: [T] = []

    for i in stride(from: length(list), to: 0, by: -1) {
        result.append(list[i - 1])
    }

    return result
}
