importClass(org.eclipse.core.runtime.Path);

function pad(n, width, z) {
  z = z || '0';
  n = n + '';
  return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
}

var project = eclipse.resources.workspace.root.getProject('euler');

var srcPkgs = project.getFolder('src').members();
var solutions = java.util.ArrayList();
for (var i = 0; i < srcPkgs.length; i++) {
  var pkg = srcPkgs[i];
  if (pkg.name != 'euler') {
    var files = pkg.members();
    for (var j = 0; j < files.length; j++) {
      if (files[j].name.indexOf('Euler') == 0) {
        solutions.add(files[j]);
      }
    }
  }
}

java.util.Collections.sort(solutions, new java.util.Comparator() {
  compare: function(o1, o2) {
    return o1.name.localeCompare(o2.name);
  }
});

var last = solutions.get(solutions.size() - 1);
var newSolnNumber = parseInt(last.name.replace(/Euler0*(.*).scala/, '$1')) + 1;
var newSolnName = "Euler" + pad(newSolnNumber, 4);

var template = project.getFile(new Path('src/euler/EulerTemplate.scalat'));
var templateContents = eclipse.resources.read(template);
templateContents = templateContents.replace("euler0000", last.parent.name);
templateContents = templateContents.replace("Euler0000", newSolnName);
//eclipse.window.alert(templateContents);

var newSolnFile = last.parent.getFile(newSolnName + ".scala");
newSolnFile.create(new java.io.ByteArrayInputStream(new java.lang.String(templateContents).getBytes()), true, null);
last.parent.refreshLocal(1, null);

var editorInput = new org.eclipse.ui.part.FileEditorInput(newSolnFile);
var editorId = org.eclipse.ui.PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(newSolnFile.name).id;
org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor(editorInput, editorId);
