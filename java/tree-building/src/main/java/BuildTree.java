import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

class BuildTree {
    TreeNode buildTree(Collection<Record> records) {
        Map<Integer, TreeNode> nodes = new HashMap<>();
        records.stream().sorted(Comparator.comparingInt(Record::getRecordId)).forEach(record -> {
            if (record.getRecordId() == 0) {
                if (record.getParentId() != 0) {
                    throw new InvalidRecordsException("Invalid Records");
                }
                nodes.put(0, new TreeNode(0));
            } else {
                if (record.getParentId() >= record.getRecordId() || record.getRecordId() > records.size() - 1) {
                    throw new InvalidRecordsException("Invalid Records");
                }

                TreeNode parentNode = nodes.get(record.getParentId());
                if (parentNode == null) {
                    throw new InvalidRecordsException("Invalid Records");
                }

                TreeNode node = new TreeNode(record.getRecordId());
                parentNode.getChildren().add(node);
                nodes.put(node.getNodeId(), node);
            }
        });
        return nodes.get(0);
    }
}
