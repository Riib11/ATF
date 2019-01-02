var paper_title = document.getElementById("title").innerText
var head_title = document.createElement("title")
head_title.innerText = paper_title
document.getElementsByTagName("head")[0].appendChild(head_title)
