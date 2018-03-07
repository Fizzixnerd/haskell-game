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
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2) uniform Material {
  vec4 diffuse_color;
  vec4 ambient_color;
  vec4 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

out VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3 light;
  float intensity;
} vs_out;

void main() {
  vec4 pos = vec4(position, 1) * camera.mv;

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(normal * mat3(camera.mv));
  int i;
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 light_vector = point_lights.lights[i].position.xyz - pos.xyz;
    vs_out.light = light_vector;
    vs_out.intensity = point_lights.lights[i].intensity;
  }

  vs_out.uv = uv;
  vs_out.pos = pos.xyz;
  vs_out.view = view_vector;
  vs_out.norm = norm;
  gl_Position = pos * camera.p;
}
