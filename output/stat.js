<<<<<<< HEAD
var txt = '{"release":["2022 Q4"],"price_uk":[-4.27],"price_london":[-4.93],"afford_uk":[-4.42],"afford_london":[-4.06]}';
=======
var txt = '{"release":["2022 Q3"],"price_uk":[1.51],"price_london":[-1.88],"afford_uk":[2],"afford_london":[-1.85]}';
>>>>>>> aa54f90fb530330940eaa6996d68b8fc1d8098ec
var obj = JSON.parse(txt);
document.getElementById("js-release").innerHTML = "Release: <br>" + obj.release;
document.getElementById("js-price-uk").innerHTML = obj.price_uk + " %";
document.getElementById("js-price-london").innerHTML = obj.price_london + " %";
document.getElementById("js-afford-uk").innerHTML = obj.afford_uk + " %";
document.getElementById("js-afford-london").innerHTML = obj.afford_london + " %";
