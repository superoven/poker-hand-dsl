# PureScript Poker Hand

This is an implementation of a simple DSL in purescript to parse and evaluate poker hands.

## Installation

You will need to install [purescript](http://www.purescript.org/) (^0.11.7), [pulp](https://github.com/purescript-contrib/pulp) and [psc-package](https://github.com/purescript/psc-package).

```
pulp --psc-package install
pulp build
```

## Usage

```
pulp repl
```

```
> import Poker
> hand (card Ten Clubs) (card Ten Spades) (card Queen Spades) (card Two Spades) (card Ten Hearts)
[Q♤ ,10♧ ,10♤ ,10♡ ,2♤ ], Three of a Kind: 10, Q high
```

## Testing

```
pulp test
```
