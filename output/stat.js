var txt = '{"release":["2019 Q4"],"price_uk":[-0.6],"price_london":[-3.26],"afford_uk":[-0.6],"afford_london":[-3.26]}';
var obj = JSON.parse(txt);
document.getElementById("js-release").innerHTML = "Release: <br>" + obj.release;
document.getElementById("js-price-uk").innerHTML = obj.price_uk + " %";
document.getElementById("js-price-london").innerHTML = obj.price_london + " %";
document.getElementById("js-afford-uk").innerHTML = obj.afford_uk + " %";
document.getElementById("js-afford-london").innerHTML = obj.afford_london + " %";
