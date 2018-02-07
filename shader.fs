#version 450 core

in VS_OUT {
  vec4 color;
} tes_color;

out vec4 color;

void main(void) {
  color = tes_color.color;
}
