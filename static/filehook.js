var fileString = "";

// when the file changes we write it to a global variable that we can get in Haskell
// and create an ImageBitmap to draw on the canvas
readFileString = function(evt) {
    let file = evt.target.files[0];
    let reader = new FileReader();
    reader.addEventListener('load', function() {
        fileString = reader.result.match(/data:.*?\/.*?;base64,(.*)/)[1];
    });
    reader.readAsDataURL(file);
    createImageBitmap(file).then(function(bm) {
        canvas.width = bm.width;
        canvas.height = bm.height;
        canvas.getContext('2d').drawImage(bm, 0, 0);
    });
}

// when the mutationobserver fires we know that the file input was created
// as long as we only create new nodes once
attachToFile = function(mutation, observer) {
    console.log("mutation")
    console.log(mutation);
    if (mutation[0].addedNodes[0].id === "file") {
        $("#file")[0].addEventListener('change', readFileString);
        observer.disconnect();
    }
}

// wait until the window is loaded to start mutationobserver on body
// could also maybe put the script in the body so that the body exists when it's run
window.onload = function() {
    let observer = new MutationObserver(attachToFile);
    let target = document.body;
    let options = { childList: true };
    observer.observe(target, options);
}

console.log("setup complete");
