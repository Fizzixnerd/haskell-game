#version 450 core

#define MAX_POINT_LIGHTS 4

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec3 tangent;
layout (location = 3) in vec2 uv;

layout (row_major, std140) uniform;

layout (binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (binding = 2) uniform Material {
  vec4 diffuse_color;
  vec4 ambient_color;
  vec4 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

out gl_PerVertex {
  vec4 gl_Position;
  float gl_PointSize;
  float gl_ClipDistance[];
  float gl_CullDistance[];
};

out VS_OUT {
  vec2 uv;
  vec3 view;
  vec3[MAX_POINT_LIGHTS] light;
} vs_out;

void main() {
  vec4 P = camera.mv * vec4(position, 1);

  vec3 N = normalize(mat3(camera.mv) * normal);
  vec3 T = normalize(mat3(camera.mv) * tangent);
  vec3 B = cross(N, T);

  int i;
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 L = point_lights.lights[i].position.xyz - P.xyz;
    vs_out.light[i] = normalize(vec3(dot(L, T), dot(L, B), dot(L, N)));
  }

  vec3 V = - P.xyz;
  vs_out.view = normalize(vec3(dot(V, T), dot(V, B), dot(V, N)));
  vs_out.uv = uv;

  gl_Position = camera.p * P;
}
