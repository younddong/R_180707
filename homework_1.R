##############################
## 1 Week (2018.07.07) #
##############################

# R 언어에는 객체타입 대표 6로
# Vector, List, Matrix, Array, Factor, Data Frame
# 이 있고... 이들은 Vector 가 확장합니다.
# Vector 는 다시 6개 기본타입에 따라
# Logical(T,F), Numeric(실수), Integer(정수),
# Complex (허수), Character (문자열), Raw (주소값)
# 로 종류가 나뉘는데 보통은
# Logical, Numeric, Character 세가지가 쓰입니다



##############################
# #
# Homework 1 #
# #
##############################
## 월요일 문제
# 체질량 지수(體質量指數, Body Mass Index, BMI)는 인간의 비만도를 나타내는 지수로,
# 체중과 키의 관계로 계산된다.
# 키가 t 미터, 체중이 w 킬로그램일 때, BMI는 다음과 같다.
# (키의 단위가 센티미터가 아닌 미터임에 유의해야 한다.)
# 공식은 https://ko.wikipedia.org/wiki/%EC%B2%B4%EC%A7%88%EB%9F%89_%EC%A7%80%EC%88%98
# 가면 구할 수 있습니다.
# 이 BMI 지수를 구하는 Function 객체를 구하시오
# 이 문제는 스위치버전은 구할수 없고 if-else 버전만 가능합니다.

getBmi <- function(t, w){
  t <- t/100
  t <- t*t
  bmi <- w/t
  if(bmi >= 35) res <- cat("고도 비만") else
    if(bmi >= 30) res <- cat("중등도 비만") else
      if(bmi >= 25) res <- cat("경도 비만") else
        if(bmi >= 23) res <- cat("과체중") else
          if(bmi >= 18.5) res <- cat("정상") else
            if(bmi < 18.5 ) res <- cat("저체중") else
              return(res)
}


getBmi <- function(t, w){
  t <- t/100
  t <- t*t
  bmi <- w/t
  return(cat(res <- if(bmi >= 35) res <- cat("고도 비만") else
    if(bmi >= 30) res <- cat("중등도 비만") else
      if(bmi >= 25) res <- cat("경도 비만") else
        if(bmi >= 23) res <- cat("과체중") else
          if(bmi >= 18.5) res <- cat("정상") else
            if(bmi < 18.5 ) res <- cat("저체중") else
              "Error"
  )
  )
}


getBmi(178, 85)



##############################
# #
# Homework 2 #
# #
##############################
## 화요일 문제
## 문자열에서 필요한 값 추출하기
# substr("문자열", 시작인덱스, 끝인덱스) 인덱스 시작은 1
# 아래 - 부분도 한자리로 인식함
stu <- "800101-1"
sub <- substr(stu, 8,8)
sub
# 실행하면 1이 찍힘
# 이것을 통해서 주민번호를 통한 성별 추출이 가능함
# 1, 3 남자 2, 4 여자 5, 6 은 외국인...그외는 잘못된 값
# 문제
# 위 문법을 이용해서 주민번호를 입력하면
# 남, 여, 외 이렇게 출력되는 객체(함수)를 완성하시오


getSex <- function(jumin){
  sex <- substr(jumin, 8, 8)
  return(cat(res <- switch(
    sex,
    '1' = "남",
    '2' = "여",
    '3' = "남",
    '4' = "여",
    '5' = "외",
    '6' = "외",
    "잘못 입력하셨습니다."
  )
  )
  )
}

getSex("800101-5")


##############################
# #
# Homework 3 #
# #
##############################
## 수요일 문제
# sample(1:3,1,replace = TRUE) 하면 1부터 3까지 중에서 랜덤숫자
# 하나를 반환합니다. 1:3 이 범위고, 1이 갯수, replace = TRUE 는 한번 뽑은
# 숫자를 다시 출력할지 여부인데 True를 주면 다시 뽑힐 수 있습니다. 즉 2
# 가 랜덤으로 나왔어도 다음 회차에서 다시 2가 나올 수 있습니다.
# 이를 이용해서 가위바위보 를 함수로 생성하세요.
# 예제는 아래와 같습니다.
# comVal <- sample(1:3,1,replace = TRUE)
# myVal <- 2
# rps <- function(comVal, myVal)
# 이김


rsp <- function(x, y){
  return(cat(x,
             " VS ",
             y,
             " --> ",
             res <- if(x > y) "왼쪽이 이김" else
               if(x == y) "비김" else
                 if(x < y) "오른쪽이 이김" else
                   "Error"
  )
  )
}

comVal <- sample(1:3,1,replace = TRUE)
myVal <- sample(1:3,1,replace = TRUE)
rsp(comVal, myVal)


