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
    - [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
  
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
     - [CodeQL](https://codeql.github.com/docs/)

- [기호 실행](https://docs.google.com/presentation/d/1_Z9-xdhx04eDvcfdLUW8Jj8_2WS9t5YJS9AO8jYdLYo/edit?usp=sharing)

  * WHILE
     - [기호실행](https://github.com/kwanghoon/Lecture_SAV/blob/master/whilelang/app/symexec/SymExec.hs)
  * 오픈소스 소프트웨어
     - [Klee](http://klee.github.io/)

### WHILE 데모 프로그램 설치 및 실행 방법

- [하스켈 stack 설치](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [구문 분석에서 기호 실행까지 분석 실행 방법]
- Z3 개발자 라이브러리를 설치
  * (우분투) 리눅스
    * sudo apt-get install libz3-dev
  * 윈도우즈 
    * [z3-4.8.12 버전](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.12)을 다운로드
    * D:\z3-4.8.12-x64-win 디렉토리 아래 bin에 라이브러리, include에 헤더 파일이 있다고 가정하고,
       * stack build --extra-include-dirs=D:\z3-4.8.12-x64-win\include --extra-lib-dirs=D:\z3-4.8.12-x64-win\bin 

```
 $ git clone https://github.com/kwanghoon/Lecture_SAV
 $ cd Lecture_SAV/whilelang
 $ stack build
 $ stack exec -- whilelang-exe --lex ./example/while2.while
 $ stack exec -- whilelang-exe --parse ./example/while2.while
 $ stack exec -- whilelang-exe --typecheck ./example/while2.while
 $ stack exec -- whilelang-exe --dataflow ./example/while2.while
 $ stack exec -- whilelang-exe --symexec ./example/while2.while
 $ stack exec -- whilelang-exe --json ./example/while2.while
```

- GHCi 안에서 동일한 명령어를 실행
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
 ghci> doJson srcFile
```

### 오픈소스 소프트웨어 도구 설치 및 실행 방법

- 각 해당 발표 자료 참고

### 만든이
 - [전남대학교 소프트웨어공학과 최광훈](https://kwanghoon.github.io)
