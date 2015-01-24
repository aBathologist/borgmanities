# Borgmanities

    We are two humans. Publish your works and share of your soul.
    Your cultural and intellectual accomplishments will be added to our own.
    Your insights will adapt to support our theses.
    Resistance is facile.

But [citation](https://www.youtube.com/watch?v=AyenRCJ_4Ww) can be expected.

This project is for fun and learning. We are currently working on a twitter bot in Haskell, but may expand in any number of directions in the future. If we had an agenda, it would include opposing borgification.

## Active Project:

- [BotticelliBot](https://github.com/aBathologist/borgmanities/wiki/BotticelliBot): a nascent twitter bot.

## Todo:

- Make the python twitter interface handle unicode:
  https://docs.python.org/2/howto/unicode.html (Interestingly, this isn't
  necessary when running the programs in iTerm. But is when running
  them in emacs-shell. I guess the former sets the environment to
  handle unicode encoding).
- Add proper logging to PaperTitleGen.IO.hs.
- Tune algorithm to produce better output:

    1. Don’t let single letters count as nouns (‘r’ and ‘s’ count as a
       nouns because they name the letter, I guess).
    2. Only take definitions before “with” and certain other
       prepositions, maybe before “having” and coordinating particles
       (“or” “and”) too
    3. How to deal with \8217? (That should mean ‘)
    4. Maybe find a better dictionary that will correctly identify nouns
             like “directionless”?
    5. Also, WordsApi is useless with plurals, so on a recent test I
       got back "manorial based jobs xlyqfwri rt dsw worldwide the
       fight" as the compliment, when I should just be truncated after
       "jobs".
    1. we should eliminate all numbers, single letters, and parentheses.

- Link twitterbotticelli app with BotticelliBot (currently linked with
  my account).
- Refactor code:
    - Eliminate "Random Selections". Belongs rather in PaperTitleGen.IO.
    - PaperTitleGen.IO is written in a primitive style, because of
      currently limited knowledge. Substantial gains in clarity can be
      made once I push to the next level.
- Build tweet from TitleParts data.
- Implement tweet posting through the python interface.

