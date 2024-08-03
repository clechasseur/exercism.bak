fun <T> List<T>.customAppend(other: List<T>): List<T> {
    val result = mutableListOf<T>()
    result.addAll(this)
    result.addAll(other)
    return result
}

fun <T> List<List<T>>.customConcat(): List<T> {
    val result = mutableListOf<T>()
    for (l in this) {
        result.addAll(l)
    }
    return result
}

fun <T> List<T>.customFilter(predicate: (T) -> Boolean): List<T> {
    val result = mutableListOf<T>()
    for (obj in this) {
        if (predicate(obj)) {
            result.add(obj)
        }
    }
    return result
}

val <T> List<T>.customSize: Int
    get() {
        var siz = 0
        for (obj in this) {
            ++siz
        }
        return siz
    }

fun <T, R> List<T>.customMap(mapper: (T) -> R): List<R> {
    val result = mutableListOf<R>()
    for (obj in this) {
        result.add(mapper(obj))
    }
    return result
}

fun <T, R> List<T>.customFoldLeft(seed: R, folder: (R, T) -> R): R {
    var value = seed
    for (obj in this) {
        value = folder(value, obj)
    }
    return value
}

fun <T, R> List<T>.customFoldRight(seed: R, folder: (T, R) -> R): R {
    var value = seed
    for (i in (customSize - 1) downTo 0) {
        value = folder(this[i], value)
    }
    return value
}

fun <T> List<T>.customReverse(): List<T> {
    val result = mutableListOf<T>()
    for (i in (customSize - 1) downTo 0) {
        result.add(this[i])
    }
    return result
}
