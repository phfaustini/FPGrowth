# FPGrowth

Haskell implementation of FP-Growth<sup>1</sup> algorithm

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

* Cabal:

```
sudo apt install haskell-platform
```

### Building

* In the root folder, type

```
cabal configure
```

```
cabal build
```


## Running the tests

* In the root folder, type

```
./dist/build/fpgrowth/fpgrowth +RTS -N1 -s -ls -M2g
```


## Author

* **Pedro Faustini**


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

## References

<sup>1</sup> Jiawei Han, Jian Pei, and Yiwen Yin. Mining frequent patterns without candidate generation. *In Proceedings of the ACM SIGMOD International on Management of Data*, pages 1–12. ACM Press, 2000.