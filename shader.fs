#version 450 core

in VS_OUT {
  vec4 color;
} vs_out;

out vec4 color;

void main(void) {
  color = vs_out.color;
}
