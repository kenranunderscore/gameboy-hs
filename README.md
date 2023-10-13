# A GameBoy emulator written in Haskell

This is a hobby project created (mostly) [on
stream](https://twitch.tv/kenran__). The idea was to learn things about
lower-level programming and the inner workings of computers.

I'm happy to report that I consider this a huge success, as not only does it
"work" , it even works reasonably fast by just using "standard" Haskell tools
like the `StateT` monad and only immutable data structures. For a little more
detail see [below](#state-of-the-implementation).

## Goals

- Be able to run a very simple game like Tetris at normal GameBoy speed, that
  is, 60 FPS
  
Everything else is sugar on top and depends on how much I like working on it :)

## Setup

In order to build the package locally, you should probably use
[Nix](https://nixos.org/), as that is the only thing that I've tested. Simply
enter a development environment:

``` shell
nix develop
```

Then you have `cabal-install`, `haskell-language-server` etc. at your disposal.

Build the emulator with `cabal build`; run it with `cabal run`, but note that
you have to pass a couple of command line arguments in order to actually start
it. For instance:

``` shell
cabal run . -- -r <some_rom_file>
```

For help on the possible options check out `cabal run . -- --help`.

## State of the implementation

The main goal of running Tetris has been reached! On my laptop, I can get up to
roughly 600 FPS, which was honestly surprising to me, given that I didn't use
anything that screams "performance". It was a bit of work getting there though,
and is only reasonably fast when using at least `-O1`.

### CPU

The CPU is complete. That is, it passes [Blargg's cpu_instrs test
suite](https://github.com/retrio/gb-test-roms).

### Memory banking

I've decided I want to implement at least MBC1, but so far there is only a
lackluster "dummy" implementation of ROM bank switching. It suffices to start up
Super Mario Land and watch the demo, but crashes after a bit of playing.

### Input

While input in general is "working", keys are sometimes seemingly dropped, which
is most likely due to the weird implementation of the communication between the
rendering thread and the main thread.

### Timers and PPU

This emulator is not very accurate w.r.t. timing. The minimum unit of time is
"one instruction", so we don't know anything about clocks or ticks that happen
during execution of single instructions. This is also (probably) a reason why
we're not passing the Blargg timer test; it doesn't even really start its suite
but fails during initialization.
