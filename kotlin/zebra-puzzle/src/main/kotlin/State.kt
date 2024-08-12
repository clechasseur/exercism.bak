class State(val houses: List<House> = defaultHouses()) {
    companion object {
        private fun defaultHouses() = List(5) { idx -> House(idx) }
    }

    fun getWith(param: String, value: String) = houses.firstOrNull { value == it.get(param) }

    fun set(house: House, param: String, value: String) {
        house.set(param, value)
        val toRemove = mutableListOf(HouseParamInfo(house, param, value))
        while (toRemove.isNotEmpty()) {
            val paramInfo = toRemove.removeAt(toRemove.size - 1)
            houses.filter { it != paramInfo.house }.forEach { h ->
                if (h.couldBe(paramInfo.param, paramInfo.value)) {
                    h.remove(paramInfo.param, paramInfo.value)
                    val singleValue = h.get(paramInfo.param)
                    if (singleValue != null) {
                        toRemove.add(HouseParamInfo(h, paramInfo.param, singleValue))
                    }
                }
            }
        }
    }

    fun remove(house: House, param: String, value: String) {
        house.remove(param, value)
        val isThen = house.get(param)
        if (isThen != null) {
            set(house, param, isThen)
        }
    }

    fun clone() = State(houses.map { it.clone() })
}

private data class HouseParamInfo(val house: House, val param: String, val value: String)
