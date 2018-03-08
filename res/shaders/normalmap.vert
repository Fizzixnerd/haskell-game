#version 450 core

#define MAX_POINT_LIGHTS 4

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec3 tangent;
layout (location = 3) in vec2 uv;

out VS_OUT {
  vec2 uv;
  vec3 view;
  vec3[MAX_POINT_LIGHTS] light;
} vs_out;

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

void main() {
  vec4 P = vec4(position, 1) * camera.mv;

  vec3 N = normalize(normal * mat3(camera.mv));
  vec3 T = normalize(tangent * mat3(camera.mv));
  vec3 B = cross(N, T);

  int i;
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 L = point_lights.lights[i].position.xyz;
    vs_out.light[i] = normalize(vec3(dot(L, T), dot(L, B), dot(L, N)));
  }

  vec3 V = - P.xyz;
  vs_out.view = normalize(vec3(dot(V, T), dot(V, B), dot(V, N)));
  vs_out.uv = uv;

  gl_Position = P * camera.p;
}
