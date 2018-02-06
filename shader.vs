#version 450 core

layout (location = 0) in vec4 pos;

out VS_OUT {
  vec4 color;
} vs_out;

void main() {
  gl_Position = pos;
  vs_out.color = vec4(0.8, 1.0, 0.0, 1.0);
}
