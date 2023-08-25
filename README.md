## [강의] 프로그램 분석 기법 - 하향식 소개


### 소개
 - 프로그램 분석 기법을 소개하는 강의
 - 분석 기법 구현보다 이해와 사용에 중점을 둔 하향식 소개 
 - 구문분석에서 동적 의미, 타입체킹, 정적 분석, 기호 실행의 주제 (향후에 퍼징을 추가할 예정)
 - WHILE 언어에 대한 각 기법을 구현 (Haskell 사용)
 - 각 분석 기법을 활용한 오픈소스 소프트웨어를 소개

### 목차
- [소프트웨어 분석 개요](https://docs.google.com/presentation/d/1sPgADP18a_4HIS__g8GoYUSFQcxB3hk8ljlBNPkVqWQ/edit?usp=sharing)

  * [WHILE 프로그래밍언어](https://github.com/kwanghoon/Lecture_SAV/tree/master/whilelang/example)

- [구문분석](https://docs.google.com/presentation/d/1_JrGo2I4U-_bO2nN5QLKZpfpicCBDcjs693UaWocQ3E/edit?usp=sharing)

  * WHILE
    - [Lexer](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/Lexer.hs)
    - [Parser](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/Parser.hs)
  * 오픈소스 소프트웨어
    - [PMD](https://pmd.github.io/), [SonarQube](https://www.sonarqube.org/)
  
- [의미 분석 - 동적 시맨틱스와 타입 체킹](https://docs.google.com/presentation/d/1Qd_yBSS9QXrNDe2xmJK3hseoTPZ8_DxPMMlusFb8PiA/edit?usp=sharing)

  * WHILE
     - [인터프리터](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/interp/Interp.hs)
     - [타입 체커](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/typecheck/Typecheck.hs)
  * 오픈소스 소프트웨어
     - [PyRight](https://github.com/microsoft/pyright)
  
- [정적 분석 - 자료 흐름 분석기](https://docs.google.com/presentation/d/1JewV7c8Q389PR7nuIVlpuytD66ZIyS-zcg3sdVQzAGo/edit?usp=sharing)

  * WHILE
     - [자료흐름분석 - Reaching Definition](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/dataflow/Dataflow.hs)
  * 오픈소스 소프트웨어
     - [FBInfer](https://fbinfer.com)

- [기호 실행](https://docs.google.com/presentation/d/1_Z9-xdhx04eDvcfdLUW8Jj8_2WS9t5YJS9AO8jYdLYo/edit?usp=sharing)

  * WHILE
     - [기호실행](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/symexec/SymExec.hs)
  * 오픈소스 소프트웨어
     - [Klee](http://klee.github.io/)

### WHILE 데모 프로그램 설치 및 실행 방법

- [하스켈 stack 설치](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [구문 분석에서 기호 실행까지 분석 실행 방법]
- (리눅스 우분투에서 빌드 - 윈도우즈의 경우 Z3 의존성 문제)

```
 $ git clone https://github.com/kwanghoon/Lecture_SAV
 $ cd Lecture_SAV/whilelang
 $ stack ghci --
 ghic> let srcFile = "./example/while2.while"
 ghci> doLexing srcFile
 ghci> doParsing srcFile
 ghci> doRun srcFile
 ghci> doTypecheck srcFile
 ghci> doAnalysis srcFile
 ghci> doSymbolic srcFile
```

### 오픈소스 소프트웨어 도구 설치 및 실행 방법

- 각 해당 발표 자료 참고

### 만든이
 - [전남대학교 소프트웨어공학과 최광훈](https://kwanghoon.github.io)