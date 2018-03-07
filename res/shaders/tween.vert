#version 450 core

#define MAX_POINT_LIGHTS 128

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

layout (std140, binding = 0) uniform Camera {
  layout (offset = 0) mat4 mvp;
  layout (offset = 64) mat4 mv;
  layout (offset = 128) mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2, align = 16) uniform Material {
  layout (offset = 0) vec4 diffuse_color;
  layout (offset = 16) vec4 ambient_color;
  layout (offset = 32) vec4 specular_color;
  layout (offset = 48) float specular_strength;
  layout (offset = 52) float specular_exponent;
} material;

out VS_OUT {
  vec3 lighting;
  vec2 uv;
} vs_out;

void main() {
  gl_Position = vec4(position, 1) * camera.mvp;
  vs_out.uv = uv;
  vs_out.lighting = vec3(0, 1, 0);
}
