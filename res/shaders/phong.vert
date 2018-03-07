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
  layout (offset = 192) vec4 diffuse_color;
  layout (offset = 208) vec4 ambient_color;
  layout (offset = 224) vec4 specular_color;
  layout (offset = 240) float specular_strength;
  layout (offset = 244) float specular_exponent;
} material;

out VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3[MAX_POINT_LIGHTS] light;
  float[MAX_POINT_LIGHTS] intensity;
} vs_out;

void main() {
  vec4 pos = vec4(position, 1) * camera.mv;

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(normal * mat3(camera.mv));

  int i;
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 light_vector = point_lights.lights[i].position.xyz - pos.xyz;
    vs_out.light[i] = light_vector;
    vs_out.intensity[i] = point_lights.lights[i].intensity;
  }

  vs_out.uv = uv;
  vs_out.pos = pos.xyz;
  vs_out.view = view_vector;
  vs_out.norm = norm;
  gl_Position = pos * camera.p;
}