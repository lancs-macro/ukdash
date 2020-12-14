var txt = '{"release":["2020 Q3"],"price_uk":[2.6],"price_london":[3.46],"afford_uk":[3.01],"afford_london":[3.58]}';
var obj = JSON.parse(txt);
document.getElementById("js-release").innerHTML = "Release: <br>" + obj.release;
document.getElementById("js-price-uk").innerHTML = obj.price_uk + " %";
document.getElementById("js-price-london").innerHTML = obj.price_london + " %";
document.getElementById("js-afford-uk").innerHTML = obj.afford_uk + " %";
document.getElementById("js-afford-london").innerHTML = obj.afford_london + " %";
