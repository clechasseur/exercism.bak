def score(word):
    return sum([SCORE_BY_LETTER[c] for c in word.upper()])

LETTERS_BY_SCORE = {
    1: ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'],
    2: ['D', 'G'],
    3: ['B', 'C', 'M', 'P'],
    4: ['F', 'H', 'V', 'W', 'Y'],
    5: ['K'],
    8: ['J', 'X'],
    10: ['Q', 'Z']
}
SCORE_BY_LETTER = {
    letter:score
    for (score, letters) in LETTERS_BY_SCORE.items()
    for letter in letters
}
