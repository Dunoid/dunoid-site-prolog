function check_form(form, tag) {
    //Takes an HTML form and verifies its input
    var name, age, ageN, blockA, blockN, vName, vAge;

    age = form.age.value;
    ageN = parseInt(age);

    name = form.name.value;

    blockA = document.getElementById("age_check" + tag);
    blockN = document.getElementById("name_check" + tag);

    if (name === "") {
        blockN.innerHTML = "No name!";
        vName = false;
    } else {
        vName = true;
    }

    if (isNaN(ageN)) {
        blockA.innerHTML = "Invalid age!";
        vAge = false;
    } else if (ageN < 3) {
        blockA.innerHTML = "You need to be at least 3 to use the Internet.";
        vAge = false;
    } else if (ageN > 150) {
        blockA.innerHTML = "You must be alive to use the Internet.";
        vAge = false;
    } else {
        vAge = true;
    }

    //Set display for warning messages
    blockA.style.display = vAge ? "none" : "inline";
    blockN.style.display = vName ? "none" : "inline";

    return vName && vAge;
}

function search_people(form, Id) {
    var n, a, blkA, blkN, results, vForm = true;

    blkA = document.getElementById("age_check" + Id);
    blkN = document.getElementById("name_check" + Id);
    results = document.getElementById("search_results");

    n = form.name.value;
    a = form.age.value;

    if (a == "" || (a !== '*' && isNaN(a))) {
        blkA.innerHTML = "You need a number or asterisk (*)";
        blkA.style.display = "inline";
        vForm = false;
    } else {
        blkA.style.display = "none";
    }
    if (n === "") {
        blkN.innerHTML = "You can't leave the name blank.";
        blkN.style.display = "inline";
        vForm = false;
    } else {
        blkN.style.display = "none";
    }

    if (vForm) {
        $.get("search_people", { name: n, age: a }, function (data, status) {
            results.innerHTML = data;
            results.style.display = "block";
        });
        //We always return false because I'm terrible
    } else {
        results.style.display = "none";
    }
    return false;
}
