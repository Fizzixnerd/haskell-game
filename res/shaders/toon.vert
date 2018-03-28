#version 450 core

#define MAX_POINT_LIGHTS 4

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec3 tangent;
layout (location = 3) in ivec4 bone_id;
layout (location = 4) in vec4 bone_weight;

layout (location = 5) in vec2 uv;

layout (row_major, std140) uniform;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

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
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3[MAX_POINT_LIGHTS] light;
  float[MAX_POINT_LIGHTS] intensity;
} vs_out;

void main() {
  vec4 pos = camera.mv * vec4(position, 1);

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(mat3(camera.mv) * normal);
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
  gl_Position = camera.p * pos;
}
