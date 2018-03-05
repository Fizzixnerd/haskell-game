#version 450 core

#define MAX_POINT_LIGHTS 128

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

layout (std140, binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

struct PointLight {
  vec3 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  // xyz is position; w is intensity.
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2) uniform Material {
  vec3 diffuse_color;
  vec3 ambient_color;
  vec3 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

out VS_OUT {
  vec3 lighting;
  vec2 uv;
} vs_out;

void main() {
  if (point_lights.num > 0) {
      gl_Position = camera.mvp * vec4(position, 1) +
        vec4(material.diffuse_color, 0) - vec4(material.diffuse_color, 0);
      vs_out.uv = uv;
      vs_out.lighting = vec3(0, 0, 0);
    }
}
