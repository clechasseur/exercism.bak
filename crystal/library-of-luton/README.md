# Library of Luton

Welcome to Library of Luton on Exercism's Crystal Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

`Char` is a data type in Crystal that represents a 32-bit [Unicode][unicode] code point.
Their size makes so they are memory efficient, and also gives it certain properties that Strings don't have.
They are created using single quotes (`'`) and do only allow for one Unicode code point to be placed inside.

```crystal
'a' # => 'a'
'a'.class # => Char
```

When working with Strings in Crystal is it quite common to work with Chars, since quite a few methods in the String class return Chars or takes Chars as arguments.
For example, the [`String#[]`][index] method returns a Char, or the [`String#chars`][chars] method returns a collection of Chars.

## Incrementing / Decrementing

Due to char being a 32 Unicode point do they support incrementing and decrementing, this makes it so if you have `'a'` so can you get `'b'` by adding one.

```crystal
'a' + 5
# => 'f'

'z' - 25
# => 'a'
```

There is also a method called [`Char#succ`][succ] which will return the next char in the Unicode table and [`Char#pred`][pred] which will return the previous char in the Unicode table.

```crystal
'a'.succ
# => 'b'

'z'.pred
# => 'y'
```

## concatenation

Since Chars is only meant to represent a 32-bit Unicode, can't 2 chars be concatenation.
Thereby Chars can only be concatenated with Strings.

```Crystal
'a' + "bc"
# => "abc"

'a' + 'b'
# => error: expected argument #1 to 'Char#+' to be Int or String, not Char
```

## Properties methods

Crystal has a few methods that can be used to get information about a Char, like if it is a number or a letter, or if it is uppercase or lowercase.
Here is a list of some of the methods, for a full list see the [Char API][char-api]:

| Method                           | Description                                          |
| -------------------------------- | ---------------------------------------------------- |
| [`Char#number?`][number]         | Returns `true` if the Char is a number               |
| [`Char#letter?`][letter]         | Returns `true` if the Char is a letter               |
| [`Char#lowercase?`][lowercase]   | Returns `true` if the Char is lowercase              |
| [`Char#uppercase?`][uppercase]   | Returns `true` if the Char is uppercase              |
| [`Char#ascii?`][ascii]           | Returns `true` if the Char is an ASCII character      |
| [`Char#whitespace?`][whitespace] | Returns `true` if the Char is a whitespace character |

## Converting

### Converting between code points and Chars

Since Chars is a 32-bit Unicode point, so can you convert between Chars and code points (Int).
This can be done using the [`Char#ord`][ord] method which will return the code point of the Char, or the [`Int#chr`][chr] method which will return the Char of the code point.

```crystal
'a'.ord
# => 97

97.chr
# => 'a'
```

### Converting between types

As with other data types so does Chars have a few methods that can be used to convert between types.
The most common one is the `Char#to_s` method which will convert the Char to a String, but there is also the `Char#to_i` method which will convert the Char to an Int, And the `Char#to_f` method which will convert the Char to a Float.

```crystal
'a'.to_s
# => "a"

'4'.to_i
# => 4

'a'.to_i
# => Error: Invalid integer: a (ArgumentError)
```

[char-api]: https://crystal-lang.org/api/latest/Char.html
[succ]: https://crystal-lang.org/api/latest/Char.html#succ%3AChar-instance-method
[pred]: https://crystal-lang.org/api/latest/Char.html#pred%3AChar-instance-method
[index]: https://crystal-lang.org/api/latest/String.html#%5B%5D%28index%3AInt%29%3AChar-instance-method
[chars]: https://crystal-lang.org/api/latest/String.html#chars%3AArray%28Char%29-instance-method
[ord]: https://crystal-lang.org/api/latest/Char.html#ord%3AInt32-instance-method
[chr]: https://crystal-lang.org/api/latest/Int.html#chr%3AChar-instance-method
[number]: https://crystal-lang.org/api/latest/Char.html#number%3F%3ABool-instance-method
[letter]: https://crystal-lang.org/api/latest/Char.html#letter%3F%3ABool-instance-method
[lowercase]: https://crystal-lang.org/api/latest/Char.html#lowercase%3F%3ABool-instance-method
[uppercase]: https://crystal-lang.org/api/latest/Char.html#uppercase%3F%3ABool-instance-method
[ascii]: https://crystal-lang.org/api/latest/Char.html#ascii%3F%3ABool-instance-method
[unicode]: https://en.wikipedia.org/wiki/Unicode
[whitespace]: https://crystal-lang.org/api/latest/Char.html#whitespace%3F%3ABool-instance-method

## Instructions

The library of Luton is a large library with a big collection of different kinds of literature varying from books to art.
The library has a section where they study literature and add books to the computer system.

There are certain parts that aim to be automated and you are asked to help them with this.
This includes helping things from automating the inscribe process forms to helping decrypting texts.

## 1. Get first letter of title

The books in a library are supposed to be easy to find and therefore they are sorted by the first letter of the book's title.

Implement the `Library.first_letter` method that takes an argument `title` which holds the title of the book.
The method should return the first letter of the title.

```Crystal
Library.first_letter("Mystery of the Crystal Skull")
# => 'M'
```

## 2. Get authors initials

The library has a lot of books and they are all written by different authors.
Often authors are referred to by their initials and therefore the library requested a system that gives the initials of the author, by inputting the author's first name and last name.
The initials should be formatted as such: `{first_letter_of_first_name}.{first_letter_of_last_name}`

Implement the `Library.initials` method that takes two arguments `first_name` and `last_name` which hold the first name and last name of the author.
The method should return the initials of the author.

```Crystal
Library.initials("William", "Shakespeare")
# => "W.S"
```

## 3. Decrypt letter

Some literature is encrypted and the library has a lot of encrypted texts.
The library would like a new decryption system for a special kind of encryption.

The encryption works by taking the letter and replacing it with the letter preceding it in the alphabet.
For example `a` becomes `z`, `b` becomes `a`, `c` becomes `b`, and so on.

The library would like it to be able to decrypt both uppercase and lowercase letters and keep the same case as the original letter.

Implement the `Library.decrypt_character` method that takes an argument `character` which holds the letter to decrypt.
The method should return the decrypted letter.

```Crystal
Library.decrypt_character('a')
# => 'z'
```

## 4. Decrypt text

The library would also like to be able to decrypt a whole text, with the encryption, that takes the preceding letter in the alphabet.

The decrypter should only decrypt letters and leave all other characters as they are.

Implement the `Library.decrypt_text` method that takes an argument `text` which holds the text to decrypt.
The method should return the decrypted text.

```Crystal
Library.decrypt_text("Ifmmp Xpsme!")
# => "Hello World!"
```

## Source

### Created by

- @meatball133

### Contributed to by

- @erikschierboom