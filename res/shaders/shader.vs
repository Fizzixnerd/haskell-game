#version 450 core

layout (location = 0) in vec4 vertexPos;
layout (location = 1) in vec2 vertexUV;
layout (location = 2) in vec3 vertexNormal;

uniform mat4 MVP;

out VS_OUT {
  vec2 UV;
} vs_out;

void main() {
  gl_Position = MVP * vertexPos;
  vs_out.UV = vertexUV;
}
