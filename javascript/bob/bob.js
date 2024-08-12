function allUppercase(input) {
  const letters = [...input];
  return letters.every((c) => c.toUpperCase() == c) && letters.some((c) => c.toLowerCase() != c);
}

export const hey = (message) => {
  const trimmed = message.trim();
  const question = trimmed.endsWith('?');
  const yell = allUppercase(trimmed);
  if (question && yell) {
    return "Calm down, I know what I'm doing!";
  } else if (question) {
    return "Sure.";
  } else if (yell) {
    return "Whoa, chill out!";
  } else if (trimmed.length == 0) {
    return "Fine. Be that way!";
  } else {
    return "Whatever.";
  }
};
