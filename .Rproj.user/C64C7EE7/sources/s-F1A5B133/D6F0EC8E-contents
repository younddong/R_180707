# 오칙연산 

1+1 # 산수
a <- 1
a
b <- 2  # 점
b
a+b # 대수학
c <- a+b
c

vec01 <- c(1,2,3,7) # 선
vec01 <- c(1L,2L,3L,7) # 선

vec01 # Object : 객체 
vec02 <- c(1:10)
vec02
vec03 <- seq(1,10)
vec03
vec04 <- 1:10
vec04
vec05 <- seq(1, 10, by=3)
vec05
class(vec05)
vec06 <- c("바", "보")
vec06
class(vec06)

vec06 <- 1:3
vec06
mean(vec06)
class(vec06)

max(vec06)
min(vec06)



## 함수 만들기 

# 1. 5 더하기 7 로직을 구하시오 
x <- 5
y <- 7
# 2. 5 더하기 7 로직을 식까지 보이도록 구하시오 
op <- "+"
eq <- "="
cat(x, op, y, eq, x+y)
## 변하지 않는 값이면 (상수) 이면 그냥 넣어도 된다
## cat(x, "+", y, "=", x+y)

# 3. 더하기 로직을 구하시오  --> function !
plus <- function(x,y){ return(x+y) }
plus(2, 7)

# 4. 더하기 로직을 식까지 보이도록 구하시오  --> function !
op_plus <- function(x,y){ return(cat(x, "+", y, "=", x+y)) }
op_plus(2, 7)

# 5. 빼기 로직을 구하시오  --> function !
minus <- function(x,y){ return(x-y) }
minus(2, 7)

# 6. 곱하기 로직을 구하시오  --> function !
multiple <- function(x,y){ return(x*y) }
multiple(2, 7)

# 7. 나누기 로직을 구하시오  --> function !
division <- function(x,y){ return(x%/%y) }
division(4, 2)

# 8. 나머지 로직을 식까지 보이도록 구하시오  --> function !
remain <- function(x,y){ return(x%%y) }
remain(4, 2)



## assignment <- : 할당
## branch : 분기문
## loop : 반복문



## if - else 구문
## 함수 만들기 전
op <- "+"
x <- 6
y <- 3
cat(x, op, y, "=", 
if(op=="+") print(x+y) else 
if(op=="-") print(x-y) else 
if(op=="*") print(x*y) else 
if(op=="/") print(x%/%y) else
if(op=="%") print(x%%y) else 
  print("Error")
)

op <- "*"
x <- 6
y <- 3
if(op=="+") print(plus(x,y)) else 
if(op=="-") print(minus(x,y)) else 
if(op=="*") print(multiple(x,y)) else 
if(op=="/") print(division(x,y)) else
if(op=="%") print(remain(x,y)) else 
  print("Error")



## switch 구문 
op <- "*"
x <- 3
y <- 4
cat(
  x,
  op,
  y,
  "=",
switch(
  op,
  '+'= toString(x + y),
  '-'= toString(x - y),
  '*'= toString(x * y),
  '/'= toString(x %/% y),
  '%'= toString(x %% y),
  "Error"
))


op <- "*"
x <- 3
y <- 4
cat(
  x,
  op,
  y,
  "=",
  switch(
    op,
    '+'= plus(x, y),
    '-'= minus(x, y),
    '*'= multiple(x, y),
    '/'= division(x, y),
    '%'= remain(x, y),
    "Error"
))


## if - else 구문 - function으로 만들기
## 
if_cals <- function(x, op, y) {
  if(op=="+") return(plus(x,y)) else 
  if(op=="-") return(minus(x,y)) else 
  if(op=="*") return(multiple(x,y)) else 
  if(op=="/") return(division(x,y)) else
  if(op=="%") return(remain(x,y)) else 
    return("Error")
}
cat(123, "*", 1234, "=", if_cals(123, "*", 1234))


printIfCals <- function() {
  return 
}



if_cals <- function(x, op, y) {
  res <- if(op=="+") plus(x,y) else 
         if(op=="-") minus(x,y) else 
         if(op=="*") multiple(x,y) else 
         if(op=="/") division(x,y) else
         if(op=="%") remain(x,y) else 
          "Error"
  return(res)
}

if_cals(123, "*", 1234)





printCals <- function(x, op, y, result) {
  return(cat(x, op, "=", y, result))
}




# switch 구문 - function으로 만들기 
sw_calc <- function(x, op, y) {
  res <- switch(
          op,
          '+'= plus(x, y),
          '-'= minus(x, y),
          '*'= multiple(x, y),
          '/'= division(x, y),
          '%'= remain(x, y),
          "Error"
  )
  return(cat(x, op, y, "=", res))
#  return(res)
}
sw_calc(4, "*", 10)



sw_calc <- function(x, op, y) {
  return(cat(x, 
             op, 
             y, 
             "=", 
             switch(
              op,
              '+'= plus(x, y),
              '-'= minus(x, y),
              '*'= multiple(x, y),
              '/'= division(x, y),
              '%'= remain(x, y),
              "Error"
            )
        )
  )
}


ls()
rm(result)
sw_calc



## 성적표
# kor, eng, math --> 총점과 평균 구하기 
# ex) grade(80, 80, 80) ---> 총점:240 평균:80
grade <- function(kor, eng, math){
  return(cat(
          "총점 : ",
          sum(kor,eng,math),
          " 평균 : ",
          mean(c(kor,eng,math))
          )
  )
}
grade(75, 60, 100)
grade(80, 80, 80)


## 성적표
# kor, eng, math --> 총점과 평균 구하기 
# ex) grade(80, 80, 80) ---> 총점:240 평균:80
# if - else / switch 
# 평균이 90 이상 A / 80 이상 B / 70 이상 C / 60 이상 D / 50 이상 E / 그 밖 F

grade_if <- function(kor, eng, math) {
  return(cat(
        "총점 : ",
        sum(kor, eng, math),
        " 평균 : ",
        mean <- mean(c(kor, eng, math)),
        " 학점 : ",
        grade <- if(mean >= 90) "A" else
                 if(mean >= 80) "B" else
                 if(mean >= 70) "C" else
                 if(mean >= 60) "D" else
                 if(mean >= 50) "E" else
                  "F"
        )
  )
}
grade_if(90,90,70)
grade_if(10,10,10)


grade_sw <- function(kor, eng, math) {
  return(cat(
        "총점 : ",
        sum <- kor+eng+math,
        " 평균 : ",
        mean <- sum%/%3,
        " 학점 : ",
        grade <- switch(
                  toString(mean%/%10),
                  '10' = "A",
                  '9' = "A",
                  '8' = "B",
                  '7' = "C",
                  '6' = "D",
                  '5' = "E",
                  "F"
                 )
          )
  )
}
grade_sw(90,90,70)
grade_sw(100,100,100)




