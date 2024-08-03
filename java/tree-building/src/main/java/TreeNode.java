import java.util.ArrayList;
import java.util.List;

final class TreeNode {
    private final int nodeId;
    private final List<TreeNode> children = new ArrayList<>();

    TreeNode(final int nodeId) {
        this.nodeId = nodeId;
    }

    int getNodeId() {
        return nodeId;
    }

    List<TreeNode> getChildren() {
        return children;
    }
}
