enum Signal {
    WINK(1), DOUBLE_BLINK(2), CLOSE_YOUR_EYES(4), JUMP(8);

    private final int value;

    Signal(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}
