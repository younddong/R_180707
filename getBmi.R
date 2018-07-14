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
  bmi <- w / ((t*t) / 10000)
  return(res <- if(bmi >= 35) "고도 비만" else
                if(bmi >= 30 & bmi < 35) "중등도 비만" else
                if(bmi >= 25 & bmi < 30) "경도 비만" else
                if(bmi >= 23 & bmi < 24.9) "과체중" else
                if(bmi >= 18.5 & bmi < 22.9) "정상" else
                  "저체중" 
  )
}

getBmi(160.6, 50)

