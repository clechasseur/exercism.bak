#include "bob.h"

#include <ctype.h>
#include <stdbool.h>
#include <string.h>

bool is_whitespace(char c);
bool is_silence(const char *greeting);
bool is_question(const char *greeting);
bool is_yelling(const char *greeting);

const char *hey_bob(const char *greeting)
{
    if (is_silence(greeting)) {
        return "Fine. Be that way!";
    }

    bool question = is_question(greeting);
    bool yelling = is_yelling(greeting);
    if (question) {
        if (yelling) {
            return "Calm down, I know what I'm doing!";
        }
        return "Sure.";
    }
    if (yelling) {
        return "Whoa, chill out!";
    }

    return "Whatever.";
}

bool is_whitespace(char c) {
    return isblank(c) || c == '\r' || c == '\n';
}

bool is_silence(const char *greeting)
{
    for (; *greeting != '\0'; ++greeting) {
        if (!is_whitespace(*greeting)) {
            return false;
        }
    }

    return true;
}

bool is_question(const char *greeting)
{
    for (const char *rev = greeting + (strlen(greeting) - 1); rev >= greeting; --rev) {
        if (*rev == '?') {
            return true;
        }
        if (!is_whitespace(*rev)) {
            return false;
        }
    }

    return false;
}

bool is_yelling(const char *greeting)
{
    bool found_upper = false;
    for (; *greeting != '\0'; ++greeting) {
        if (islower(*greeting)) {
            return false;
        }
        if (isupper(*greeting)) {
            found_upper = true;
        }
    }

    return found_upper;
}
