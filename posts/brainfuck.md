---
title: why you should try brainfuck
date: 2025-06-07
description: Exploring the joy and challenges of programming in Brainfuck, an esoteric programming language that makes you think differently about code.
tags: programming, esolang
---

```brainfuck
>+++++++++[<++++++++>-] <. ---. +++++++.. +++.
```

## Background

I first came across Brainfuck last year by one of Nicuevo's streams solving AOC 2024 day 7, wrote my first program and left it, until recently I got back and it felt like meeting your long lost brother (at least for a while). The whole purpose of writing this blog is to share that joy with anyone who reads this. It also serves as a reminder if you have already explored Brainfuck previously and left it on the way to make a debut back or an invitation for someone who hasn't heard of it before to get their hands muddy.

## Why

A bit of context first: Brainfuck is a low level esoteric Turing complete language built by Urban Mueller in 1993 while he was doing his undergrad in physics. What does it look like?

```brainfuck
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>-]>>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```

Now the question is why? The real reason as quoted by Mueller was to implement a compiler—yes not the ones that you make as part of the C courses, (it was eventually 170 bytes in total), but for me the real reason is still **TO HAVE FUN**. Where is the fun in writing `useState` or `import torch`? We often don't think and just blurt out the syntax with your keyboard, but Brainfuck makes you think—activate more neurons than usual.

The next time you're a bit bored: write hello world in Brainfuck, treat it as a brainteaser and try to effectively figure out 4 different ways of doing the same thing. I won't give a tutorial on how to do that here (there are many already) but instead convince you in 4 points to do the same.

1. **It's fun** - it's not about building something life impacting or useful (most of the times)
2. **Use your brain** - when you write something in a specific way and go to bed, you're bound to think about the other n ways to do the same thing more efficiently. Over time I felt I did these while writing the first attempt.
3. **Low level** - take some break from your high level friend, go low level—back to the memory, it's like a programming detox
4. **Learn how to code** - write good and structured code, something that is declining and would help in high level languages too

> **FAIR WARNING:** Brainfuck has a threshold value that is different for everyone—don't overdo it if you realize your capacity is less or it gets really boring pretty fast. Yes it's also used professionally, one of the examples being—Brainfuck Enterprise Solutions, but it's probably not for the "normal" to say. Personally I think this is useless and totally misses the point but I have huge respect for anyone who pulls it off.

A few helpful links are dropped below and if you still aren't convinced probably you are doing all of this for the wrong reasons.

```brainfuck
+[----->+++<]>+.++++++++..-----------.-[--->+<]>-.++[--->++<]>.+++++++++.+[-->+++<]>+.++++++++.
```

## Helpful Links:

- [Brainfuck for dummies](https://gist.github.com/roachhd/dce54bec8ba55fb17d3a)
- [Intro to Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
- [Brainfuck interpreter](https://copy.sh/brainfuck/)
- [Wiki](https://en.wikipedia.org/wiki/Brainfuck)

## Cool Links:

- [Mueller's talk on BF](#)
- [Nicuveo's AOC 2024 in BF](#)
- [Brainfuck to Go compiler](#)
- [Get good at Brainfuck (at your own risk)](#)
- [BF tic tac toe](#)
