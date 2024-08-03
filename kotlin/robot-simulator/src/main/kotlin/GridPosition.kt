data class GridPosition(val x: Int, val y: Int) {
    fun move(orientation: Orientation): GridPosition = when (orientation) {
        Orientation.NORTH -> GridPosition(this.x, this.y + 1)
        Orientation.SOUTH -> GridPosition(this.x, this.y - 1)
        Orientation.EAST -> GridPosition(this.x + 1, this.y)
        Orientation.WEST -> GridPosition(this.x - 1, this.y)
    }
}
