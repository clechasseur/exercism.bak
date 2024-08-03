import java.util.List;

class RelationshipComputer<T> {
    Relationship computeRelationship(List<T> left, List<T> right) {
        if (left.size() > right.size()) {
            return computeRelationship(right, left) == Relationship.SUBLIST
                    ? Relationship.SUPERLIST : Relationship.UNEQUAL;
        } else if (left.size() == right.size()) {
            return left.equals(right) ? Relationship.EQUAL : Relationship.UNEQUAL;
        }
        for (int i = 0; i < right.size() - left.size() + 1; i++) {
            if (right.subList(i, i + left.size()).equals(left)) {
                return Relationship.SUBLIST;
            }
        }
        return Relationship.UNEQUAL;
    }
}