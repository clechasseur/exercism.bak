import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

class Dominoes {
    List<Domino> formChain(List<Domino> dominoes) throws ChainNotFoundException {
        if (dominoes.isEmpty()) {
            return Collections.emptyList();
        }
        List<Domino> chainStart = new ArrayList<>(Collections.singletonList(dominoes.get(0)));
        List<Domino> chain = continueChain(chainStart, dominoes.subList(1, dominoes.size()));
        if (chain == null) {
            throw new ChainNotFoundException("No domino chain found.");
        }
        return chain;
    }

    private List<Domino> continueChain(List<Domino> chain, List<Domino> dominoes) {
        if (dominoes.isEmpty()) {
            if (chain.get(0).getLeft() == chain.get(chain.size() - 1).getRight()) {
                return chain;
            } else {
                return null;
            }
        }

        Domino last = chain.get(chain.size() - 1);
        List<Domino> possibilities = dominoes.stream()
                .filter(d -> d.getLeft() == last.getRight() || d.getRight() == last.getRight())
                .collect(Collectors.toList());
        for (Domino possibility : possibilities) {
            Domino realPossibility = possibility.getLeft() == last.getRight()
                    ? possibility
                    : new Domino(possibility.getRight(), possibility.getLeft());
            List<Domino> newChain = new ArrayList<>(chain);
            newChain.add(realPossibility);
            List<Domino> remainingDominoes = new ArrayList<>(dominoes);
            remainingDominoes.remove(possibility);
            List<Domino> endChain = continueChain(newChain, remainingDominoes);
            if (endChain != null) {
                return endChain;
            }
        }
        return null;
    }
}