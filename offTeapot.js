var m = new Mushroom();
var teapotTriangles = m.triangles;
var teapotBC = m.bc;

function MakeCanvas(width, height, locID) {

    if (width == undefined || width < 0) {
       width = 300;
    }

    if (height == undefined || height < 0) {
       height = 300;
    }

    var canvas = document.createElement('canvas')
        canvas.tabIndex = 0;
        canvas.height = height;
        canvas.width = width;
	canvas.style.border = "1px solid #0000FF";

    if(locID == undefined) {
        document.body.appendChild(canvas);
    } else {
        div = document.getElementById(locID);
        if (null == div) {
            document.body.appendChild(canvas);
        } else {
            div.appendChild(canvas);
        }
    }

    document.body.appendChild(canvas);
    return canvas;
}

function InitGL(canvas) {
    var gl =  WebGLUtils.setupWebGL(canvas,'OES_standard_derivatives');
    if (!gl) {
        alert ("WebGL isn't available");
    }

    // required to turn on fwidth and such.
    gl.getExtension('OES_standard_derivatives');
    return gl;
}

function Canvas(width, height, locID) {
    var canvas = MakeCanvas(width, height, locID);

    var gl = InitGL(canvas);
    this.gl = gl;

    var tmpCanvas = this;
    this.x = canvas.offsetLeft;
    this.y = canvas.offsetTop;

    canvas.addEventListener("keypress",
         function(evnt) {
            tmpCanvas.KeyFunc(tmpCanvas, evnt);
         }
    );

    gl.viewport(0,0, width, height);

    var program = initShaders(gl, "vertex-shader","fragment-shader");
    gl.useProgram(program);

    this.vPos = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vPos);

    var vPos = gl.getAttribLocation(program, "vPosition");
    gl.vertexAttribPointer(vPos,3,gl.FLOAT, false,0,0);
    gl.enableVertexAttribArray(vPos);

    this.vBC = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vBC);

    var vBC = gl.getAttribLocation(program, "vBC");
    gl.vertexAttribPointer(vBC,3,gl.FLOAT, false,0,0);
    gl.enableVertexAttribArray(vBC);

    this.shaderLoc = gl.getUniformLocation(program, "vFillType");
    gl.uniform1f(this.shaderLoc, 0.0);

    this.transformLoc = gl.getUniformLocation(program, "Transform");

    this.Init();

    return this;
}

Canvas.prototype = {

    Init: function() {
        this.gl.clearColor(1.0, 1.0, 1.0, 1.0);

        this.gl.enable(this.gl.BLEND);
	this.gl.blendFunc(this.gl.SRC_ALPHA, this.gl.ONE_MINUS_SRC_ALPHA);

        this.gl.enable(this.gl.DEPTH_TEST);
	this.gl.depthFunc(this.gl.LESS);
	this.gl.depthMask(this.gl.TRUE);

	this.RestartList();

        this.shaderChoice = false;
        this.gl.uniform1f(this.shaderLoc, 0.0);

        this.Reset();
	this.Redisplay();
    },

    Reset: function() {
	this.xr = 0;
	this.yr = 0;
	this.zr = 0;

         // make these switchable


        this.gl.enable(this.gl.CULL_FACE);
        this.direction = this.gl.CCW;
        this.gl.frontFace(this.direction);
	this.gl.cullFace(this.gl.BACK);
    },

    FlipDir: function() {
        if (this.direction == this.gl.CCW) {
	    this.direction = this.gl.CW;
	} else {
	    this.direction = this.gl.CCW;
	}
        this.gl.frontFace(this.direction);
    },

    RestartList: function() {

	this.verts= teapotTriangles ;
	this.bc = teapotBC 

        this.UpdateBuffers();
    },

    UpdateBuffers: function() {
        var gl = this.gl;

        // change the vertex data
        gl.bindBuffer(gl.ARRAY_BUFFER, this.vBC);
	gl.bufferData(gl.ARRAY_BUFFER,flatten(this.bc),gl.STATIC_DRAW);

        gl.bindBuffer(gl.ARRAY_BUFFER, this.vPos);
	gl.bufferData(gl.ARRAY_BUFFER,flatten(this.verts),gl.STATIC_DRAW);

    },

    ChangeShader: function() { 
        this.shaderChoice = ! this.shaderChoice;
	var shader = 0.0;

	if (this.shaderChoice) {
	    shader = 1.0
	}
        this.gl.uniform1f(this.shaderLoc, shader);
    },

    ChangeFace: function(value) {
       switch(value) {
          default:
          case '0':
              this.gl.enable(this.gl.CULL_FACE);
	      this.gl.cullFace(this.gl.BACK);
	      break;
	  case '1':
              this.gl.enable(this.gl.CULL_FACE);
	      this.gl.cullFace(this.gl.FRONT);
	      break;
	  case '2':
              this.gl.enable(this.gl.CULL_FACE);
	      this.gl.cullFace(this.gl.FRONT_AND_BACK);
	      break;
	  case '3':
              this.gl.disable(this.gl.CULL_FACE);
	      break;
       }
    },

    KeyFunc: function(me, evnt) {
       switch(evnt.key) {
           case 'X':  me.xr += -5; break;
	   case 'Y':  me.yr += -5; break;
	   case 'Z':  me.zr += -5; break;
           case 'x':  me.xr += 5; break;
	   case 'y':  me.yr += 5; break;
	   case 'z':  me.zr += 5; break;
	   case 'r':  me.Reset(); break;

       }
       me.Redisplay();
    },

    Redisplay: function() {
	var transform = mat4(1);

        // this probably doesn't belong here
	transform = mult(transform, rotate(this.xr, [1,0,0]));
	transform = mult(transform, rotate(this.yr, [0,1,0]));
	transform = mult(transform, rotate(this.zr, [0,0,1]));
        this.gl.uniformMatrix4fv(this.transformLoc,false, flatten(transform ));

        this.gl.clear(this.gl.COLOR_BUFFER_BIT | this.gl.DEPTH_BUFFER_BIT);

	this.gl.drawArrays(this.gl.TRIANGLES, 0, this.verts.length);
        return;
    }
};
