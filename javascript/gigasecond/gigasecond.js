const GIGAMILLISECOND = 10**12;

export const gigasecond = (date) => new Date(date.getTime() + GIGAMILLISECOND);
