final class Record {
    private final int recordId;
    private final int parentId;

    public Record(final int recordId, final int parentId) {
        this.recordId = recordId;
        this.parentId = parentId;
    }

    int getParentId() {
        return parentId;
    }

    int getRecordId() {
        return recordId;
    }
}
